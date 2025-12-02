package wildcat.assembler.parser

import wildcat.{AluFunct3, AluFunct7, BranchFunct3, C_funct2, C_funct3, C_funct4, C_funct6, C_opcode, LoadStoreFunct3, Opcode, TagFunct3, dTag}
import wildcat.assembler.compiler.{Location, WorkflowParserError}
import wildcat.assembler.lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object WorkflowParser extends Parsers {
  override type Elem = TOKEN

  class WorkflowTokenReader(tokens: Seq[TOKEN]) extends Reader[TOKEN] {
    override def first: TOKEN = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[TOKEN] = new WorkflowTokenReader(tokens.tail)
  }


  def apply(tokens: Seq[TOKEN]): Either[WorkflowParserError, List[InstructionAST]] = {
    // Filter out comments and normalize newlines
    val preProcessedTokens = tokens
      .filterNot(_.isInstanceOf[COMMENT])  // Remove comments
      .foldLeft(List.empty[TOKEN]) {       // Collapse multiple newlines
        case (acc, token) => (acc.lastOption, token) match {
          case (Some(_: NEWLINE), _: NEWLINE) => acc  // Skip consecutive newlines
          case (_, token) => acc :+ token              // Keep all other tokens
        }
      }
      .dropWhile(_.isInstanceOf[NEWLINE])      // Remove leading newlines
      .reverse.dropWhile(_.isInstanceOf[NEWLINE]).reverse // Remove trailing newlines
    print(preProcessedTokens)
    val reader = new WorkflowTokenReader(preProcessedTokens)
    print("\n")
    program(reader) match {
      case NoSuccess(msg, next) => Left(WorkflowParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  def program: Parser[List[InstructionAST]] = {
    phrase(block)
  }

  def block: Parser[List[InstructionAST]] =
    repsep(statement, NEWLINE())




  // Main entry point
  def statement: Parser[InstructionAST] = positioned {

      label_def             |
      local_label_def       |
      rTypeInstruction      |
      iTypeInstruction      |
      sTypeInstruction      |
      bTypeInstruction      |
      jTypeInstruction      |
      uTypeInstruction      |
      fenceTypeInstruction  |
      // Compressed
      crTypeInstruction     |
      ciTypeInstruction     |
      cssTypeInstruction    |
      ciwTypeInstruction    |
      clTypeInstruction     |
      csTypeInstruction     |
      caTypeInstruction     |
      cbTypeInstruction     |
      cjTypeInstruction     |
      // Fallback for unimplemented instructions
      (unimplementedInstruction)
  }

  // ============================================================================
  // INSTRUCTION PARSERS
  // ============================================================================

  // R-Type: add rd, rs1, rs2
  def rTypeInstruction: Parser[RTypeAST] = positioned {
    accept("R-type instruction", {
      case LITERAL(name) if isRType(name) => name
    }) ~ register ~ COMMA() ~ register ~ COMMA() ~ register ^^ {
      case instr ~ rd ~ _ ~ rs1 ~ _ ~ rs2 =>
        val funct7 = instr match {
          case "add" | "sll" | "slt" | "sltu" | "xor" | "srl" | "or" | "and" => AluFunct7.DEFAULT
          case "sub" | "srp" | "sra" => AluFunct7.SRP_SUB
          case "mul" | "mulh" | "mulhu" | "mulhsu" | "div" | "divu" | "rem" | "remu" => AluFunct7.MUL_DIV
        }
        val funct3 = instr match {
          case "add" | "sub"  => AluFunct3.F3_ADD_SUB
          case "srp"          => AluFunct3.F3_SLL_SRP
          case "sll"          => AluFunct3.F3_SLL_SRP
          case "slt"          => AluFunct3.F3_SLT
          case "sltu"         => AluFunct3.F3_SLTU
          case "xor"          => AluFunct3.F3_XOR
          case "srl" | "sra"  => AluFunct3.F3_SRL_SRA
          case "or"           => AluFunct3.F3_OR
          case "and"          => AluFunct3.F3_AND

          case "mul"    => AluFunct3.F3_MUL
          case "mulh"   => AluFunct3.F3_MULH
          case "mulhu"  => AluFunct3.F3_MULHU
          case "mulhsu" => AluFunct3.F3_MULHSU
          case "div"    => AluFunct3.F3_DIV
          case "divu"   => AluFunct3.F3_DIVU
          case "rem"    => AluFunct3.F3_REM
          case "remu"   => AluFunct3.F3_REMU
        }
        RTypeAST(rd=rd.loc, funct3=funct3, rs1=rs1.loc, rs2=rs2.loc, funct7=funct7)
    }
  }

  private def isRType(name: String): Boolean = name match {
    case "add" | "sub" | "srp" | "sll" | "slt" | "sltu" | "xor" | "srl" | "sra" | "or" | "and" | "mul" | "mulh" | "mulhu" | "mulhsu" | "div" | "divu" | "rem" | "remu" => true
    case _ => false
  }

  // I-Type (all variants)
  def iTypeInstruction: Parser[ITypeAST] = positioned {
    immediateArithmeticInstruction | loadInstruction | jumpLinkRegInstruction | systemInstruction | castInstruction
  }

  def immediateArithmeticInstruction: Parser[ImmArithAST] = positioned {
    accept("immediate arithmetic instruction", {
      case LITERAL(name) if isImmArith(name) => name
    }) ~ register ~ COMMA() ~ register ~ COMMA() ~ immediate ^^ {
      case instr ~ rd ~ _ ~ rs1 ~ _ ~ imm =>
        val func7_imm = instr match {
          case "srpi" | "srai" => (AluFunct7.SRP_SUB << 5) | (imm.num & 0b11111)
          case _ => imm.num
        }
        val funct3 = instr match {
          case "addi" => AluFunct3.F3_ADD_SUB
          case "srpi" => AluFunct3.F3_SLL_SRP
          case "slli" => AluFunct3.F3_SLL_SRP
          case "sltiu" => AluFunct3.F3_SLTU
          case "slti" => AluFunct3.F3_SLT
          case "xori" => AluFunct3.F3_XOR
          case "srli" | "srai" => AluFunct3.F3_SRL_SRA
          case "ori" => AluFunct3.F3_OR
          case "andi" => AluFunct3.F3_AND
        }
        ImmArithAST(rd=rd.loc, funct3=funct3, rs1=rs1.loc, imm=func7_imm)
    }
  }

  private def isImmArith(name: String): Boolean = name match {
    case "addi" | "srpi" | "slli" | "sltiu" | "slti" | "xori" | "srli" | "srai" | "ori" | "andi" => true
    case _ => false
  }

  def loadInstruction: Parser[LoadAST] = positioned {
    accept("load instruction", {
      case LITERAL(name) if isLoad(name) => name
    }) ~ register ~ COMMA() ~ immediate ~ LPAREN() ~ register ~ RPAREN() ^^ {
      case instr ~ rd ~ _ ~ offset ~ _ ~ rs1 ~ _ =>
        val funct3 = instr match {
          case "lb"   => LoadStoreFunct3.LB
          case "lh"   => LoadStoreFunct3.LH
          case "lw"   => LoadStoreFunct3.LW
          case "lbu"  => LoadStoreFunct3.LBU
          case "lhu"  => LoadStoreFunct3.LHU
          case "lwu"  => LoadStoreFunct3.LWU
          case "ltr"   => TagFunct3.LTR
        }
        val opcode = instr match {
          case "ltr"  => Opcode.Tag
          case _      => Opcode.Load
        }
        LoadAST(opcode=opcode, rd=rd.loc, funct3=funct3, rs1=rs1.loc, imm=offset.num)
    }
  }

  private def isLoad(name: String): Boolean = name match {
    case "lb" | "lh" | "lw" | "lbu" | "lhu" | "lwu" | "ltr" => true
    case _ => false
  }

  def castInstruction: Parser[CastAST] = positioned {
    accept("cast instruction", {
      case LITERAL(name) if isCast(name) => name
    }) ~ register ~ COMMA() ~ register ~ COMMA() ~ iTag ^^ {
      case instr ~ rd ~ _  ~ rs1 ~ _ ~ tag =>
        val funct3 = instr match {
          case "cast" => TagFunct3.CAST
          case "cast.t" => TagFunct3.CAST_T
      }
        CastAST(rd = rd.loc, funct3 = funct3,rs1 = rs1.loc, imm = tag.encoded)
    }
  }

  private def isCast(name: String): Boolean = name match {
    case "cast" | "cast.t" => true
    case _ => false
  }

  def jumpLinkRegInstruction: Parser[JumpLinkRegAST] = positioned {
    accept("jump and link register instruction", {
      case LITERAL("jalr") => "jalr"
    }) ~ register ~ COMMA() ~ register ~ COMMA() ~ immediate ^^ {
      case _ ~ rd ~ _ ~ rs1 ~ _ ~ imm =>
        JumpLinkRegAST(rd=rd.loc, rs1=rs1.loc, imm=imm.num)
    }
  }

  def systemInstruction: Parser[SystemAST] = positioned {
    accept("system instruction", {
      case LITERAL("ecall") => "ecall"
      case LITERAL("ebreak") => "ebreak"
    }) ^^ {
      case "ecall" => SystemAST(imm = 0b0)
      case "ebreak" => SystemAST(imm = 0b1)
    }
  }

  // S-Type: sw rs2, offset(rs1)
  def sTypeInstruction: Parser[STypeAST] = positioned {
    accept("S-type instruction", {
      case LITERAL(name) if isSType(name) => name
    }) ~ register ~ COMMA() ~ immediate ~ LPAREN() ~ register ~ RPAREN() ^^ {
      case instr ~ rs2 ~ _ ~ offset ~ _ ~ rs1 ~ _ =>
        val funct3 = instr match {
          case "st" => LoadStoreFunct3.ST
          case "sb" => LoadStoreFunct3.SB
          case "sh" => LoadStoreFunct3.SH
          case "sw" => LoadStoreFunct3.SW
          case "str" => TagFunct3.STR
        }
        val opcode = instr match {
          case "str"  => Opcode.Tag
          case _      => Opcode.Store
        }
        STypeAST(opcode=opcode, funct3 = funct3, rs1=rs1.loc, rs2=rs2.loc, offset=offset.num)
    }
  }

  private def isSType(name: String): Boolean = name match {
    case "st" | "sb" | "sh" | "sw" | "str" => true
    case _ => false
  }

  // B-Type: beq rs1, rs2, label
  def bTypeInstruction: Parser[BTypeAST] = positioned {
    accept("B-type instruction", {
      case LITERAL(name) if isBType(name) => name
    }) ~ register ~ COMMA() ~ register ~ COMMA() ~ target ^^ {
      case instr ~ rs1 ~ _ ~ rs2 ~ _ ~ trgt =>
        val funct3 = instr match {
          case "beq" => BranchFunct3.BEQ
          case "bne" => BranchFunct3.BNE
          case "blt" => BranchFunct3.BLT
          case "bge" => BranchFunct3.BGE
          case "bltu" => BranchFunct3.BLTU
          case "bgeu" => BranchFunct3.BGEU
        }
        if (trgt.isInstanceOf[TImm]) {

        }
        BTypeAST(funct3 = funct3, rs1 = rs1.loc, rs2 = rs2.loc, target = trgt)
    }
  }

  private def isBType(name: String): Boolean = name match {
    case "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => true
    case _ => false
  }

  // J-Type: jal rd, label
  def jTypeInstruction: Parser[JTypeAST] = positioned {
    accept("J-type instruction", {
      case LITERAL("jal") => "jal"
    }) ~ register ~ COMMA() ~ target ^^ {
      case _ ~ rd ~ _ ~ lbl =>
        JTypeAST(rd = rd.loc, target = lbl)
    }
  }

  // U-Type: lui/auipc rd, imm
  def uTypeInstruction: Parser[UTypeAST] = positioned {
    accept("U-type instruction", {
      case LITERAL("lui") => "lui"
      case LITERAL("auipc") => "auipc"
    }) ~ register ~ COMMA() ~ immediate ^^ {
      case "lui" ~ rd ~ _ ~ imm => LoadUpperImmAST(rd = rd.loc, imm = imm.num << 12)
      case "auipc" ~ rd ~ _ ~ imm => AddUpperImmAST(rd = rd.loc, imm = imm.num << 12)
    }
  }

  // FENCE-Type: fence pred, succ
  def fenceTypeInstruction: Parser[FenceTypeAST] = positioned {
    accept("fence instruction", {
      case LITERAL("fence") => "fence"
    }) ~ predecessor ~ COMMA() ~ successor ^^ {
      case _ ~ pred ~ _ ~ succ =>
        val pred_access = parseAccessType(pred.set)
        val succ_access = parseAccessType(succ.set)
        FenceTypeAST(pred = pred_access, succ = succ_access)
    }
  }



  // ============================================================================
  // COMPRESSED INSTRUCTION PARSERS
  // ============================================================================

  // CR-Type:
  def crTypeInstruction: Parser[CRTypeAST] = positioned {
    accept("CR-type instruction", {
      case LITERAL(name) if isCRType(name) => name
    }) ~ opt(register ~ opt(COMMA() ~ register)) ^^ {
      case instr ~ optRegs =>
        val funct4 = instr match {
          case "c.mv" => C_funct4.C_MV
          case "c.add" => C_funct4.C_ADD
          case "c.jr" => C_funct4.C_JR
          case "c.jalr" => C_funct4.C_JALR
          case "c.ebreak" => C_funct4.C_EBREAK
        }

        val (rd_rs1Value, rs2Value) = optRegs match {
          case Some(rd_rs1 ~ Some(_ ~ rs2)) => (rd_rs1.loc, rs2.loc)  // Two registers
          case Some(rd_rs1 ~ None) => (rd_rs1.loc, 0)                 // One register
          case None => (0, 0)                                         // No registers
        }

        CRTypeAST(funct4 = funct4, rd_rs1 = rd_rs1Value, rs2 = rs2Value)
    }
  }

  private def isCRType(name: String): Boolean = name match {
    case "c.mv" | "c.add" | "c.jr" | "c.jalr" | "c.ebreak" => true
    case _ => false
  }

  // CI-Type:
  def ciTypeInstruction: Parser[CITypeAST] = positioned {
    accept("CI-type instruction", {
      case LITERAL(name) if isCIType(name) => name
    }) ~ opt(register ~ COMMA()) ~ (immediate | iTag ) ^^ {
      case instr ~ optReg ~ imm_tag =>
        val imm = imm_tag match {
          case i: IMMEDIATE => i
          case t: ITAG      => IMMEDIATE(t.name, t.encoded)
        }
        val (opcode, funct3, imm12, imm2_6) = instr match {
          case "c.lwsp"     => (C_opcode.C_LWSP,      C_funct3.C_LWSP,   (imm.num >> 5) & 0b1, (((imm.num >> 2) & 0b111) << 2) | ((imm.num >> 6) & 0b11))
          case "c.li"       => (C_opcode.C_LI,        C_funct3.C_LI,        (imm.num >> 5) & 0b1, imm.num & 0b11111)
          case "c.lui"      => (C_opcode.C_LUI,       C_funct3.C_LUI,       (imm.num >> 5) & 0b1, imm.num & 0b11111)
          case "c.addi"     => (C_opcode.C_ADDI,      C_funct3.C_ADDI,      (imm.num >> 5) & 0b1, imm.num & 0b11111)
          case "c.addi16sp" => (C_opcode.C_ADDI16SP,  C_funct3.C_ADDI16SP,  (imm.num >> 9) & 0b1, (((imm.num >> 4) & 0b1) << 4) | (((imm.num >> 6) & 0b1) << 3) | (((imm.num >> 7) & 0b11) << 1) | ((imm.num >> 5) & 0b1))
          case "c.slli"     => (C_opcode.C_SLLI,      C_funct3.C_SLLI,      (imm.num >> 5) & 0b1, imm.num & 0b11111)
          case "c.cast"     => (C_opcode.C_CAST,      C_funct3.C_CAST,      (imm.num >> 5) & 0b1, imm.num & 0b11111)
        }

        val (rd_rs1Value) = optReg match {
          case Some(rd_rs1 ~ _) => (rd_rs1.loc)               // One register
          case None => (0x2)                                  // No registers
        }

        CITypeAST(opcode = opcode, funct3 = funct3, rd_rs1 = rd_rs1Value, imm12 = imm12, imm2_6 = imm2_6)
    }
  }

  private def isCIType(name: String): Boolean = name match {
    case "c.lwsp" | "c.li" | "c.lui" | "c.addi" | "c.addi16sp" | "c.slli" | "c.cast" => true
    case _ => false
  }

  // CSS-Type:
  def cssTypeInstruction: Parser[CSSTypeAST] = positioned {
    accept("CSS-type instruction", {
      case LITERAL(name) if isCSSType(name) => name
    }) ~ register ~ COMMA() ~ immediate ^^ {
      case instr ~ rs2 ~ _ ~ imm =>
        val (opcode, funct3, imm7_12) = instr match {
          case "c.stsp"     => (C_opcode.C_STSP,  C_funct3.C_STSP, (((imm.num >> 2) & 0b1111) << 2) | ((imm.num >> 6) & 0b11))
        }
        CSSTypeAST(opcode = opcode, funct3 = funct3, rs2 = rs2.loc, imm7_12 = imm7_12)
    }
  }

  private def isCSSType(name: String): Boolean = name match {
    case "c.stsp" => true
    case _ => false
  }

  // CIW-Type:
  def ciwTypeInstruction: Parser[CIWTypeAST] = positioned {
    accept("CIW-type instruction", {
      case LITERAL(name) if isCIWType(name) => name
    }) ~ cRegister ~ COMMA() ~ immediate ^^ {
      case instr ~ rd ~ _ ~ imm =>
        val (opcode, funct3, imm5_12) = instr match {
          case "c.addi4spn"     => (C_opcode.C_ADDI4SPN,  C_funct3.C_ADDI4SPN, (((imm.num >> 4) & 0b11) << 6) | (((imm.num >> 6) & 0b1111) << 2) | (((imm.num >> 2) & 0b1) << 1) | ((imm.num >> 3) & 0b1))
        }
        CIWTypeAST(opcode = opcode, funct3 = funct3, rd = rd.loc, imm5_12 = imm5_12)
    }
  }

  private def isCIWType(name: String): Boolean = name match {
    case "c.addi4spn" => true
    case _ => false
  }

  // CL-Type:
  def clTypeInstruction: Parser[CLTypeAST] = positioned {
    accept("CL-type instruction", {
      case LITERAL(name) if isCLType(name) => name
    }) ~ cRegister ~ COMMA() ~ immediate ~ LPAREN() ~ cRegister ~ RPAREN() ^^ {
      case instr ~ rd ~ _ ~ offset ~ _ ~ rs1 ~ _ =>
        val (opcode, funct3, imm5_6, imm10_12) = instr match {
          case "c.lw"     => (C_opcode.C_LW,  C_funct3.C_LW, (((offset.num >> 6) & 0b1) << 1) | ((offset.num >> 2) & 0b1), (offset.num >> 3) & 0b111)
        }
        CLTypeAST(opcode = opcode, funct3 = funct3, rd = rd.loc, rs1 = rs1.loc, imm5_6 = imm5_6, imm10_12 = imm10_12)
    }
  }

  private def isCLType(name: String): Boolean = name match {
    case "c.lw" => true
    case _ => false
  }

  // CS-Type:
  def csTypeInstruction: Parser[CSTypeAST] = positioned {
    accept("CS-type instruction", {
      case LITERAL(name) if isCSType(name) => name
    }) ~ cRegister ~ COMMA() ~ immediate ~ LPAREN() ~ cRegister ~ RPAREN() ^^ {
        case instr ~ rs2 ~ _ ~ offset ~ _ ~ rs1 ~ _ =>
          val (opcode, funct3, imm5_6, imm10_12) = instr match {
            case "c.st"  => (C_opcode.C_ST,  C_funct3.C_ST,   (((offset.num >> 6) & 0b1) << 1) | ((offset.num >> 2) & 0b1), (offset.num >> 3) & 0b111)
          }
          CSTypeAST(opcode = opcode, funct3 = funct3, rs1 = rs1.loc, rs2 = rs2.loc, imm5_6 = imm5_6, imm10_12 = imm10_12)
    }
  }

  private def isCSType(name: String): Boolean = name match {
    case "c.st" => true
    case _      => false
  }

  // CA-Type:
  def caTypeInstruction: Parser[CATypeAST] = positioned {
    accept("CA-type instruction", {
      case LITERAL(name) if isCAType(name) => name
    }) ~ cRegister ~ COMMA() ~ cRegister ^^ {
        case instr ~ rd_rs1 ~ _ ~ rs2 =>
          val (opcode, funct6, funct2) = instr match {
            case "c.and" => (C_opcode.C_AND, C_funct6.C_AND,   C_funct2.C_AND)
            case "c.or"  => (C_opcode.C_OR,  C_funct6.C_OR,   C_funct2.C_OR)
            case "c.xor" => (C_opcode.C_XOR, C_funct6.C_XOR,  C_funct2.C_XOR)
            case "c.sub" => (C_opcode.C_SUB, C_funct6.C_SUB,  C_funct2.C_SUB)
          }
          CATypeAST(opcode = opcode, funct6 = funct6, rd_rs1 = rd_rs1.loc, funct2 = funct2, rs2 = rs2.loc)
    }
  }

  private def isCAType(name: String): Boolean = name match {
    case "c.and" | "c.or" | "c.xor" | "c.sub" => true
    case _ => false
  }

  // CB-Type:
  def cbTypeInstruction: Parser[CBTypeAST] = positioned {
    cBranchInstruction | cbArithInstruction
  }

  def cbArithInstruction: Parser[CBArithmTypeAST] = positioned {
    accept("CB-type instruction", {
      case LITERAL(name) if isCBArith(name) => name
    }) ~ cRegister ~ COMMA() ~ immediate ^^ {
      case instr ~ rd_rs1 ~ _ ~ offset =>
        val (opcode, funct3, imm2_6, imm10_12) = instr match {
          case "c.srpi"     => (C_opcode.C_SRPI,  C_funct3.C_SRPI,  offset.num & 0b11111, (((offset.num >> 5) & 0b1) << 2) | 0b00)
          case "c.srli"     => (C_opcode.C_SRLI,  C_funct3.C_SRLI,  offset.num & 0b11111, (((offset.num >> 5) & 0b1) << 2) | 0b00)
          case "c.srai"     => (C_opcode.C_SRAI,  C_funct3.C_SRAI,  offset.num & 0b11111, (((offset.num >> 5) & 0b1) << 2) | 0b01)
          case "c.andi"     => (C_opcode.C_ANDI,  C_funct3.C_ANDI,  offset.num & 0b11111, (((offset.num >> 5) & 0b1) << 2) | 0b10)
        }
        CBArithmTypeAST(opcode = opcode, funct3 = funct3, rd_rs1 = rd_rs1.loc, imm2_6 = imm2_6, imm10_12 = imm10_12)
    }
  }

  private def isCBArith(name: String): Boolean = name match {
    case "c.srli" | "c.srai" | "c.andi" => true
    case _ => false
  }

  def cBranchInstruction: Parser[CBranchTypeAST] = positioned {
    accept("CB-type instruction", {
      case LITERAL(name) if isCBranch(name) => name
    }) ~ cRegister ~ COMMA() ~ target ^^ {
      case instr ~ rd_rs1 ~ _ ~ trgt =>
        val (opcode, funct3) = instr match {
          case "c.beqz"     => (C_opcode.C_BEQZ,  C_funct3.C_BEQZ)
          case "c.bnez"     => (C_opcode.C_BNEZ,  C_funct3.C_BNEZ)
        }
        CBranchTypeAST(opcode = opcode, funct3 = funct3, rd_rs1 = rd_rs1.loc, target = trgt)
    }
  }

  private def isCBranch(name: String): Boolean = name match {
    case "c.beqz" | "c.bnez" => true
    case _ => false
  }

  // CJ-Type:
  def cjTypeInstruction: Parser[CJTypeAST] = positioned {
    accept("CJ-type instruction", {
      case LITERAL(name) if isCJType(name) => name
    }) ~ target ^^ {
      case instr ~ trgt =>
        val (opcode, funct3) = instr match {
          case "c.j"    => (C_opcode.C_J,   C_funct3.C_J)
          case "c.jal"  => (C_opcode.C_JAL, C_funct3.C_JAL)
        }

        CJTypeAST(opcode = opcode, funct3 = funct3, target = trgt)
    }
  }

  private def isCJType(name: String): Boolean = name match {
    case "c.j" | "c.jal" => true
    case _ => false
  }

  def unimplementedInstruction: Parser[InstructionAST] = positioned {
    rep1(anyTokenButNewline) >> {
      case tokens =>
        val tokenList = tokens.mkString(" ")

        failure(
          s"""Unrecognized or unimplemented instruction: $tokenList
             |
             |Possible issues:
             |  - Instruction '$tokenList' is not implemented yet
             |  - Incorrect syntax or operand format
             |  - Typo in instruction name
             |
             |Tokens parsed: ${tokens.mkString(", ")}
             |""".stripMargin
        )
    }
  }

  def anyTokenButNewline: Parser[TOKEN] = accept("any non-newline token", {
    case t: TOKEN if t != NEWLINE() => t
  })


  // ============================================================================
  // HELPER PARSERS - Context-sensitive token recognition
  // ============================================================================

  // Accept any LITERAL as instruction mnemonic (validation happens in instruction parsers)
  private def literal: Parser[LITERAL] = positioned {
    accept("instruction mnemonic", { case lit@LITERAL(name) => lit })
  }

  // Accept LITERAL if it's a valid register name
  private def register: Parser[REGISTER] = positioned {
    accept("register", {
      case lit@LITERAL(name) if registerLocation.isDefinedAt(name) =>
        REGISTER(name, registerLocation(name))
    })
  }

  private def registerLocation: PartialFunction[String, Int] = {
    case s"x$n" if n.forall(_.isDigit) && n.toInt >= 0 && n.toInt <= 31 => n.toInt
    case "zero" => 0
    case "ra" => 1
    case "sp" => 2
    case "gp" => 3
    case "tp" => 4
    case "t0" => 5
    case "t1" => 6
    case "t2" => 7
    case "s0" | "fp" => 8
    case "s1" => 9
    case "a0" => 10
    case "a1" => 11
    case "a2" => 12
    case "a3" => 13
    case "a4" => 14
    case "a5" => 15
    case "a6" => 16
    case "a7" => 17
    case "s2" => 18
    case "s3" => 19
    case "s4" => 20
    case "s5" => 21
    case "s6" => 22
    case "s7" => 23
    case "s8" => 24
    case "s9" => 25
    case "s10" => 26
    case "s11" => 27
    case "t3" => 28
    case "t4" => 29
    case "t5" => 30
    case "t6" => 31
  }

  // Compressed helpers
  private def cRegister: Parser[REGISTER] = positioned {
    accept("register", {
      case lit@LITERAL(name) if cRegisterLocation.isDefinedAt(name) =>
        REGISTER(name, cRegisterLocation(name))
    })
  }

  private def cRegisterLocation: PartialFunction[String, Int] = {
    case "s0" | "x8"  => 0b000
    case "s1" | "x9"  => 0b001
    case "a0" | "x10" => 0b010
    case "a1" | "x11" => 0b011
    case "a2" | "x12" => 0b100
    case "a3" | "x13" => 0b101
    case "a4" | "x14" => 0b110
    case "a5" | "x15" => 0b111
  }

  private def immediate: Parser[IMMEDIATE] = positioned {
    literalAsImmediate
  }

  private def immediateToken: Parser[IMMEDIATE] = accept("immediate", {
    case imm@IMMEDIATE(raw, num) => imm
  })

  private def literalAsImmediate: Parser[IMMEDIATE] = accept("literal as immediate", {
    case lit@LITERAL(raw) if canParseAsImmediate(raw) =>
      IMMEDIATE(raw, parseImmediate(raw))
  })

  private def canParseAsImmediate(s: String): Boolean = {
    s.matches("-?(0x[0-9a-fA-F]+|0b[01]+|[0-9]+)")
  }

  private def parseImmediate(str: String): Int = {
    val hex = "-?0x([0-9a-fA-F]+)".r
    val bin = "-?0b([01]+)".r
    val int = "(-?[0-9]+)".r

    str match {
      case hex(n) => Integer.parseInt(n, 16) * (if (str.startsWith("-")) -1 else 1)
      case bin(n) => Integer.parseInt(n, 2) * (if (str.startsWith("-")) -1 else 1)
      case int(n) => n.toInt
    }
  }

  // Accept IDENT tokens from lexer OR LITERAL that looks like fence set
  private def predecessor: Parser[PREDECESSOR] = positioned {
    accept("fence predecessor", {
      case lit @ LITERAL(name) if isFenceSet(name) => PREDECESSOR(name)
    })
  }

  private def successor: Parser[SUCCESSOR] = positioned {
    accept("fence successor", {
      case lit @ LITERAL(name) if isFenceSet(name) => SUCCESSOR(name)
    })
  }

  private def isFenceSet(s: String): Boolean = {
    s.matches("i?o?r?w?") && s.distinct == s
  }

  def parseAccessType(accessType: String): Int = {
    accessType.foldLeft(0) {
      case (acc, 'w') => acc | 0b0001
      case (acc, 'r') => acc | 0b0010
      case (acc, 'o') => acc | 0b0100
      case (acc, 'i') => acc | 0b1000
      case (acc, _) => acc
    }
  }

  def iTag: Parser[ITAG] = accept("literal as iTag", {
    case lit@LITERAL(name) if iTagEncoder.isDefinedAt(name.toLowerCase) =>
      ITAG(name, iTagEncoder(name.toLowerCase))
  })


  private def iTagEncoder: PartialFunction[String, Int] = {
    case "u_byte" => dTag.UNSIGNED_BYTE
    case "s_byte" => dTag.SIGNED_BYTE
    case "u_half" => dTag.UNSIGNED_HALF
    case "s_half" => dTag.SIGNED_HALF
    case "u_word" => dTag.UNSIGNED_WORD
    case "s_word" => dTag.SIGNED_WORD
    case "none"   => dTag.NONE
  }
  // ============================================================================
  // TARGET PARSERS (for branch/jump destinations)
  // ============================================================================

  private def target: Parser[Target] = positioned {
    local_label_ref | immediate_ref | ident
  }

  // Accept IDENT tokens OR LITERAL as identifier
  private def ident: Parser[Target] = positioned {
    accept("identifier", {
      case lit@LITERAL(name) => TSymbol(name)
    })
  }

  // LOCAL_LABEL_REF is produced by lexer
  private def local_label_ref: Parser[Target] = positioned {
    accept("local label reference", {
      case ref@LOCAL_LABEL_REF(n, forward) => TLocal(n, forward)
    })
  }

  // Accept immediate as target (for numeric offsets)
  private def immediate_ref: Parser[Target] = positioned {
    accept("immediate as target", {
      case imm @ IMMEDIATE(_, num) => TImm(num)
    }) |
      accept("immediate as target", {
        case lit@LITERAL(raw) if canParseAsImmediate(raw) =>
          TImm(parseImmediate(raw))
      })
  }

  // ============================================================================
  // LABEL DEFINITIONS (produced by lexer)
  // ============================================================================

  private def label_def: Parser[InstructionAST] = positioned {
    accept("label definition", {
      case lbl@LABEL_DEF(name) => LabelDef(lbl.name)
    })
  }

  private def local_label_def: Parser[InstructionAST] = positioned {
    accept("local label definition", {
      case lbl@LOCAL_LABEL_DEF(n) => LocalLabelDef(lbl.n)
    })
  }
}
