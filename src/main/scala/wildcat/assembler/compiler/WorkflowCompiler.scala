package wildcat.assembler.compiler

import wildcat.assembler.lexer.WorkflowLexer
import wildcat.assembler.parser._


object WorkflowCompiler {
  /**
   * Compiles RISC-V assembly code to machine code
   * @param code Assembly source code
   * @return Either compilation error or array of 32-bit instructions
   */
  def apply(code: String): Either[WorkflowCompilationError, Array[Int]] = {
    for {
      tokens <- WorkflowLexer(code).left.map(e => WorkflowLexerError(e.location,e.msg))
      ast <- WorkflowParser(tokens).left.map(e => WorkflowParserError(e.location,e.msg))
      binary <- compile(ast)
    } yield binary
  }

  /**
   * Two-pass compilation:
   * Pass 1: Build symbol table (label -> address mapping)
   * Pass 2: Encode instructions using symbol table
   */
  private def compile(ast: List[InstructionAST]): Either[WorkflowCompilationError, Array[Int]] = {
    try {
      // Pass 1: Build symbol table
      val symbols = buildSymbolTable(ast)

      // Pass 2: Encode instructions
      val instructions = ast.filterNot(isLabel) // Filter out label definitions
      val bin = new Array[Int](instructions.length)

      var pc: Int = 0
      for (instruction <- instructions) {
        instruction match {
          case c: CompressedAST =>
            val wordIndex = pc >> 2
            val byteOffset = (((pc & 0b11) >> 1) << 1) * 8
            bin(wordIndex) = (RISCVEncoder.encode(instruction, pc, symbols) << byteOffset) | bin(wordIndex)
            pc += 2
          case _ =>
            val byteOffset = ((pc & 0b11) >> 1) << 1
            if (byteOffset == 2) {
              pc +=2
            }
            val wordIndex = pc >> 2
            bin(wordIndex) = RISCVEncoder.encode(instruction, pc, symbols)
            pc += 4
        }

      }

      Right(bin)
    } catch {
      case e: Exception => Left(CompilationError(e.getMessage))
    }
  }

  /**
   * Pass 1: Build symbol table mapping labels to addresses
   */
  private def buildSymbolTable(ast: List[InstructionAST]): Map[String, Int] = {
    var address = 0
    var symbols = Map.empty[String, Int]
    var localLabels = Map.empty[Int, Int] // local label number -> address

    for (instruction <- ast) {
      instruction match {
        case LabelDef(name) =>
          symbols = symbols + (name -> address)
        case LocalLabelDef(n) =>
          localLabels = localLabels + (n -> address)
        case _ =>
          address += 4 // Each instruction is 4 bytes
      }
    }

    symbols
  }

  /**
   * Check if an instruction is a label definition (doesn't generate code)
   */
  private def isLabel(inst: InstructionAST): Boolean = inst match {
    case LabelDef(_) | LocalLabelDef(_) => true
    case _ => false
  }
}

/**
 * Encodes RISC-V instruction AST nodes into 32-bit machine code
 */
object RISCVEncoder {

  /**
   * Main encoding entry point
   * @param ast The instruction AST node
   * @param pc Current program counter (for resolving relative branches/jumps)
   * @param symbols Symbol table mapping labels to addresses
   * @return 32-bit encoded instruction
   */
  def encode(ast: InstructionAST, pc: Int = 0, symbols: Map[String, Int] = Map.empty): Int = ast match {
    case r: RTypeAST => encodeRType(r)
    case i: ImmArithAST => encodeIType(i)
    case l: LoadAST => encodeIType(l)
    case c: CastAST => encodeIType(c)
    case j: JumpLinkRegAST => encodeIType(j)
    case s: SystemAST => encodeIType(s)
    case b: BTypeAST => encodeBType(b, pc, symbols)
    case j: JTypeAST => encodeJType(j, pc, symbols)
    case s: STypeAST => encodeSType(s)
    case u: LoadUpperImmAST => encodeUType(u)
    case u: AddUpperImmAST => encodeUType(u)
    case f: FenceTypeAST => encodeFenceType(f)

    // Compressed 16-bit instructions
    case cr: CRTypeAST => encodeCRType(cr)
    case ci: CITypeAST => encodeCIType(ci)
    case css: CSSTypeAST => encodeCSSType(css)
    case ciw: CIWTypeAST => encodeCIWType(ciw)
    case cl: CLTypeAST => encodeCLType(cl)
    case cs: CSTypeAST => encodeCSType(cs)
    case ca: CATypeAST => encodeCAType(ca)
    case cb: CBTypeAST => encodeCBType(cb, pc, symbols)
    case cj: CJTypeAST => encodeCJType(cj, pc, symbols)

    case LabelDef(_) | LocalLabelDef(_) =>
      throw new Exception("Labels should be filtered out before encoding")

  }

  // ============================================================================
  // R-Type: opcode[6:0] | rd[11:7] | funct3[14:12] | rs1[19:15] | rs2[24:20] | funct7[31:25]
  // ============================================================================
  private def encodeRType(r: RTypeAST): Int = {
    r.opcode |
      (r.rd << 7) |
      (r.funct3 << 12) |
      (r.rs1 << 15) |
      (r.rs2 << 20) |
      (r.funct7 << 25)
  }

  // ============================================================================
  // I-Type: opcode[6:0] | rd[11:7] | funct3[14:12] | rs1[19:15] | imm[31:20]
  // ============================================================================
  private def encodeIType(i: ITypeAST): Int = {
    i.opcode |
      (i.rd << 7) |
      (i.funct3 << 12) |
      (i.rs1 << 15) |
      ((i.imm & 0xFFF) << 20) // 12-bit immediate, sign-extended
  }

  // ============================================================================
  // S-Type: opcode[6:0] | imm[4:0][11:7] | funct3[14:12] | rs1[19:15] | rs2[24:20] | imm[11:5][31:25]
  // ============================================================================
  private def encodeSType(s: STypeAST): Int = {
    val imm4_0 = s.offset & 0x1F        // bits [4:0]
    val imm11_5 = (s.offset >> 5) & 0x7F // bits [11:5]

    s.opcode |
      (imm4_0 << 7) |
      (s.funct3 << 12) |
      (s.rs1 << 15) |
      (s.rs2 << 20) |
      (imm11_5 << 25)
  }

  // ============================================================================
  // B-Type: opcode[6:0] | imm[11][7] | imm[4:1][11:8] | funct3[14:12] | rs1[19:15] | rs2[24:20] | imm[10:5][30:25] | imm[12][31]
  // Branch offset is relative to PC, in multiples of 2 bytes
  // ============================================================================
  private def encodeBType(b: BTypeAST, pc: Int, symbols: Map[String, Int]): Int = {
    val offset = resolveTarget(b.target, pc, symbols)
    val relOffset = offset - pc // Relative offset from current PC

    // B-type encodes a 13-bit signed offset (bit 0 is implicit 0)
    // Format: imm[12|10:5|4:1|11] scrambled into instruction
    val imm12 = (relOffset >> 12) & 0x1
    val imm11 = (relOffset >> 11) & 0x1
    val imm10_5 = (relOffset >> 5) & 0x3F
    val imm4_1 = (relOffset >> 1) & 0xF

    b.opcode |
      (imm11 << 7) |
      (imm4_1 << 8) |
      (b.funct3 << 12) |
      (b.rs1 << 15) |
      (b.rs2 << 20) |
      (imm10_5 << 25) |
      (imm12 << 31)
  }

  // ============================================================================
  // J-Type: opcode[6:0] | rd[11:7] | imm[19:12][19:12] | imm[11][20] | imm[10:1][30:21] | imm[20][31]
  // Jump offset is relative to PC, in multiples of 2 bytes
  // ============================================================================
  private def encodeJType(j: JTypeAST, pc: Int, symbols: Map[String, Int]): Int = {
    val offset = resolveTarget(j.target, pc, symbols)
    val relOffset = offset - pc // Relative offset from current PC

    // J-type encodes a 21-bit signed offset (bit 0 is implicit 0)
    // Format: imm[20|10:1|11|19:12] scrambled into instruction
    val imm20 = (relOffset >> 20) & 0x1
    val imm19_12 = (relOffset >> 12) & 0xFF
    val imm11 = (relOffset >> 11) & 0x1
    val imm10_1 = (relOffset >> 1) & 0x3FF

    j.opcode |
      (j.rd << 7) |
      (imm19_12 << 12) |
      (imm11 << 20) |
      (imm10_1 << 21) |
      (imm20 << 31)
  }

  // ============================================================================
  // U-Type: opcode[6:0] | rd[11:7] | imm[31:12][31:12]
  // Upper immediate instructions (LUI, AUIPC)
  // ============================================================================
  private def encodeUType(u: UTypeAST): Int = {
    // U-type immediate is already in upper 20 bits
    u.opcode |
      (u.rd << 7) |
      (u.imm & 0xFFFFF000) // Keep upper 20 bits
  }

  // ============================================================================
  // FENCE-Type: opcode[6:0] | rd[11:7] | funct3[14:12] | rs1[19:15] | succ[23:20] | pred[27:24] | fm[31:28]
  // ============================================================================
  private def encodeFenceType(f: FenceTypeAST): Int = {
    f.opcode |
      (f.rd << 7) |
      (f.funct3 << 12) |
      (f.rs1 << 15) |
      (f.succ << 20) |
      (f.pred << 24) |
      (f.fm << 28)
  }


  // ============================================================================
  // COMPRESSED INSTRUCTIONS (16-bit)
  // ============================================================================

  // CR-Type: funct4[15:12] | rd/rs1[11:7] | rs2[6:2] | op[1:0]
  // Format: Register
  private def encodeCRType(cr: CRTypeAST): Int = {
    cr.opcode |
      (cr.rs2 << 2) |
      (cr.rd_rs1 << 7) |
      (cr.funct4 << 12)
  }

  // CI-Type: funct3[15:13] | imm[12] | rd/rs1[11:7] | imm[6:2] | op[1:0]
  // Format: Immediate
  private def encodeCIType(ci: CITypeAST): Int = {
    ci.opcode |
      (ci.imm2_6 << 2) |
      (ci.rd_rs1 << 7) |
      (ci.imm12 << 12) |
      (ci.funct3 << 13)
  }

  // CSS-Type: funct3[15:13] | imm[12:7] | rs2[6:2] | op[1:0]
  // Format: Stack-relative Store
  private def encodeCSSType(css: CSSTypeAST): Int = {
    css.opcode |
      (css.rs2 << 2) |
      (css.imm7_12 << 7) |
      (css.funct3 << 13)
  }

  // CIW-Type: funct3[15:13] | imm[12:5] | rd'[4:2] | op[1:0]
  // Format: Wide Immediate
  private def encodeCIWType(ciw: CIWTypeAST): Int = {
    ciw.opcode |
      (ciw.rd << 2) |
      (ciw.imm5_12 << 5) |
      (ciw.funct3 << 13)
  }

  // CL-Type: funct3[15:13] | imm[12:10] | rs1'[9:7] | imm[6:5] | rd'[4:2] | op[1:0]
  // Format: Load
  private def encodeCLType(cl: CLTypeAST): Int = {
    cl.opcode |
      (cl.rd << 2) |
      (cl.imm5_6 << 5) |
      (cl.rs1 << 7) |
      (cl.imm10_12 << 10) |
      (cl.funct3 << 13)
  }

  // CS-Type: funct3[15:13] | imm[12:10] | rs1'[9:7] | imm[6:5] | rs2'[4:2] | op[1:0]
  // Format: Store
  private def encodeCSType(cs: CSTypeAST): Int = {
    cs.opcode |
      (cs.rs2 << 2) |
      (cs.imm5_6 << 5) |
      (cs.rs1 << 7) |
      (cs.imm10_12 << 10) |
      (cs.funct3 << 13)
  }

  // CA-Type: funct6[15:10] | rd'/rs1'[9:7] | funct2[6:5] | rs2'[4:2] | op[1:0]
  // Format: Arithmetic
  private def encodeCAType(ca: CATypeAST): Int = {
    ca.opcode |
      (ca.rs2 << 2) |
      (ca.funct2 << 5) |
      (ca.rd_rs1 << 7) |
      (ca.funct6 << 10)
  }

  // CB-Type: funct3[15:13] | offset[12:10] | rd'/rs1'[9:7] | offset[6:2] | op[1:0]
  // Format: Branch
  private def encodeCBType(cb: CBTypeAST, pc: Int, symbols: Map[String, Int]): Int = {
    // Note: For compressed branches, you'll need to handle offset resolution
    // For now, assuming offset is pre-calculated
    val (imm2_6, imm10_12) = cb match {
      case b: CBranchTypeAST =>
        val offset = resolveTarget(b.target, pc, symbols)
        val relOffset = offset - pc // Relative offset from current PC
        (
          (((relOffset >> 6) & 0b11) << 3) | (((relOffset >> 1) & 0b11) << 1) | ((relOffset >> 5) & 0b1),
          (((relOffset >> 8) & 0b1) << 2) | ((relOffset >> 3) & 0b11)
        )
      case a: CBArithmTypeAST => (a.imm2_6, a.imm10_12)
    }
    cb.opcode |
      (imm2_6 << 2) |
      (cb.rd_rs1 << 7) |
      (imm10_12 << 10) |
      (cb.funct3 << 13)
  }

  // CJ-Type: funct3[15:13] | jump_target[12:2] | op[1:0]
  // Format: Jump
  private def encodeCJType(cj: CJTypeAST, pc: Int, symbols: Map[String, Int]): Int = {
    val offset = resolveTarget(cj.target, pc, symbols)
    val relOffset = offset - pc // Relative offset from current PC

    val jump =
      (((relOffset >> 11) & 0b1) << 10)   |
        (((relOffset >> 4) & 0b1) << 9)     |
        (((relOffset >> 8) & 0b11) << 7)    |
        (((relOffset >> 10) & 0b1) << 6)    |
        (((relOffset >> 6) & 0b1) << 5)     |
        (((relOffset >> 7) & 0b1) << 4)     |
        (((relOffset >> 1) & 0b111) << 1)   |
        ((relOffset >> 5) & 0b1)

    cj.opcode |
      (jump << 2) |
      (cj.funct3 << 13)
  }




  // ============================================================================
  // Helper: Resolve target to absolute address
  // ============================================================================
  private def resolveTarget(target: Target, pc: Int, symbols: Map[String, Int]): Int = target match {
    case TSymbol(name) =>
      symbols.getOrElse(name, throw new Exception(s"Undefined symbol: $name"))
    case TLocal(n, forward) =>
      // Local labels need special handling - would need local label table
      throw new Exception(s"Local label resolution not implemented: $n${if (forward) "f" else "b"}")
    case TImm(bytes) =>
      pc + bytes // Treat as relative offset
  }

  // ============================================================================
  // Utility: Convert instruction to hex string
  // ============================================================================
  def toHex(instruction: Int): String = {
    f"0x${instruction}%08x"
  }

  // ============================================================================
  // Utility: Convert 16-bit compressed instruction to hex string
  // ============================================================================
  def toHex16(instruction: Int): String = {
    f"0x${instruction & 0xFFFF}%04x"
  }

  // ============================================================================
  // Utility: Convert instruction to binary string (with spacing)
  // ============================================================================
  def toBinary(instruction: Int): String = {
    instruction.toBinaryString.reverse.padTo(32, '0').reverse
      .grouped(16).mkString(" ")
  }

  // ============================================================================
  // Utility: Convert 16-bit compressed instruction to binary string
  // ============================================================================
  def toBinary16(instruction: Int): String = {
    (instruction & 0xFFFF).toBinaryString.reverse.padTo(16, '0').reverse
      .grouped(4).mkString(" ")
  }
}

// ============================================================================
// Example usage
// ============================================================================
object CompilerExample {
  def main(args: Array[String]): Unit = {
    val code =
      """# Basic arithmetic and movement
        |c.li x1, 5
        |addi x4, x3, 1
        |c.li x2, 10
        |c.add x1, x2        # x1 = x1 + x2 = 15
        |c.mv x3, x1         # x3 = x1 = 15
        |
        |# Stack operations
        |c.addi16sp -16      # sp = sp - 16
        |c.swsp x1, 0        # Store x1 to stack
        |c.lwsp x4, 0        # Load from stack to x4
        |
        |# Compressed loads/stores (uses x8-x15 only)
        |c.lw x8, 0(x9)
        |c.sw x10, 4(x11)
        |
        |# Arithmetic on compressed regs
        |c.sub x8, x9
        |c.and x10, x11
        |c.or x12, x13
        |
        |# Shifts and logic
        |c.slli x5, 2
        |c.srli x8, 1
        |c.andi x9, 0x7
        |
        |# Branches and jumps
        |loop:
        |    c.addi x1, 1
        |    c.bnez x10, loop
        |c.j end
        |
        |end:
        |    c.jr x1         # Return
        |""".stripMargin

    //val tokenized_code = for {
    //  tokens <- WorkflowLexer(code).left.map(e => WorkflowLexerError(e.location,e.msg))
    //} yield tokens
//
    //print(tokenized_code)

    WorkflowCompiler(code) match {
      case Right(binary) =>
        println("Compiled successfully!")
        binary.zipWithIndex.foreach { case (instr, i) =>
          val addr = i * 4
          println(f"0x$addr%04x: ${RISCVEncoder.toHex(instr)}: ${RISCVEncoder.toBinary(instr)}")
        }
      case Left(error) =>
        println(s"Compilation failed: ${error}")
    }
  }
}


