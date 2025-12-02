package wildcat.assembler

import wildcat.CSR._
import wildcat.CSRFunct3._
import wildcat.InstrType._
import wildcat._

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.sys.process._
import scala.util.matching.Regex


object BinaryInstructionFormats {

    // R-type: funct7 rs2 rs1 funct3 rd opcode
    @inline def rType(funct7: Int, rs2: Int, rs1: Int, funct3: Int, rd: Int, opcode: Int): Int = {
        (funct7 << 25) |
        (rs2    << 20) |
        (rs1    << 15) |
        (funct3 << 12) |
        (rd     << 7 ) |
        (opcode      )
    }

    // I-type: imm[11:0] rs1 funct3 rd opcode
    @inline def iType(imm: Int, rs1: Int, funct3: Int, rd: Int, opcode: Int): Int = {
        (imm      << 20) |
        (rs1      << 15) |
        (funct3   << 12) |
        (rd       << 7 ) |
        (opcode        )
    }

    // S-type: needs braces
    @inline def sType(imm: Int, rs2: Int, rs1: Int, funct3: Int, opcode: Int): Int = {
        val imm11_5 = (imm >> 5) & 0x7F
        val imm4_0  = (imm) & 0x1F

        (imm11_5    << 25) |
        (rs2        << 20) |
        (rs1        << 15) |
        (funct3     << 12) |
        (imm4_0     <<  7) |
        (opcode          )
    }

    // B-type: needs braces
    @inline def bType(imm: Int, rs2: Int, rs1: Int, funct3: Int, opcode: Int): Int = {
        val imm12   = (imm >> 12) & 0x1
        val imm10_5 = (imm >> 5) & 0x3F
        val imm4_1  = (imm >> 1) & 0x1F
        val imm11   = (imm >> 11) & 0x1

        (imm12      << 31) |
        (imm10_5    << 25) |
        (rs2        << 20) |
        (rs1        << 15) |
        (funct3     << 12) |
        (imm4_1     <<  8) |
        (imm11      <<  7) |
        (opcode          )
    }

    // U-type: imm[31:12] rd opcode
    @inline def uType(imm: Int, rd: Int, opcode: Int): Int = {
        (imm     << 12) |
        (rd      << 7 ) |
        (opcode       )
    }

    // J-type: imm[20|10:1|11|19:12] rd opcode
    @inline def jType(imm: Int, rd: Int, opcode: Int): Int = {
        val imm20       = (imm >> 20) & 0x1
        val imm10_1     = (imm >> 1) & 0x3FF
        val imm11       = (imm >> 11) & 0x1
        val imm19_12    = (imm >> 1) & 0xFF

        (imm20    << 31) |
        (imm10_1  << 21) |
        (imm11    << 20) |
        (imm19_12 << 12) |
        (rd       << 7 ) |
        (opcode        )
    }

    // FENCE-type: fm pred succ rs1 func3 rd opcode
    @inline def fenceType(pred: Int, succ: Int, opcode: Int): Int = {
        (pred   << 24) |
        (succ   << 20) |
        (opcode      )
    }
}

class Assembler {
    // Assemblers
    def assembleRegisterType(instr: String, rd: Int, rs1: Int, rs2: Int): Int = {

        // Define the funct7 field
        val funct7 = instr match {
            case "sub" | "sra"  => AluFunct7.SRP_SUB
            case _              => AluFunct7.DEFAULT
        }

        // Define the funct3 field
        val funct3 = instr match {
            case "add" | "sub"  => AluFunct3.F3_ADD_SUB
            case "sll"          => AluFunct3.F3_SLL_SRP
            case "slt"          => AluFunct3.F3_SLT
            case "sltu"         => AluFunct3.F3_SLTU
            case "xor"          => AluFunct3.F3_XOR
            case "srl" | "sra"  => AluFunct3.F3_SRL_SRA
            case "or"           => AluFunct3.F3_OR
            case "and"          => AluFunct3.F3_AND
        }

        // Define the opcode
        val opcode = Opcode.Alu

        // Assemble the instruction
        BinaryInstructionFormats.rType(funct7, rs2, rs1, funct3, rd, opcode)
    }

    def assembleImmediateType(instr: String, rd: Int, rs1: Int, imm: Int): Int = {

        // Define the funct3 field
        val funct3 = instr match {
            case "addi"             => AluFunct3.F3_ADD_SUB
            case "slli"             => AluFunct3.F3_SLL_SRP
            case "slti"             => AluFunct3.F3_SLT
            case "sltiu"            => AluFunct3.F3_SLTU
            case "xori"             => AluFunct3.F3_XOR
            case "srli" | "srai"    => AluFunct3.F3_SRL_SRA
            case "ori"              => AluFunct3.F3_OR
            case "andi"             => AluFunct3.F3_AND
            case "jalr"             => 0x00
            case "ecall"            => 0x00
            case "ebreak"           => 0x00
        }

        // Define the opcode
        val opcode = instr match {
            case "jalr"             => Opcode.JalR
            case "ecall" | "ebreak" => Opcode.System
            case _                  => Opcode.AluImm
        }

        val _imm = instr match {
            case "srai" => (0x20 << 5) | (imm & 0x1F)
            case _ => imm
        }

        // Assemble the instruction
        BinaryInstructionFormats.iType(_imm, rs1, funct3, rd, opcode)
    }

    def assembleLoadType(instr: String, rd: Int, imm: Int, rs1: Int): Int = {

        // Define the funct3 field
        val funct3 = instr match {
            case "lb"   => LoadStoreFunct3.LB
            case "lh"   => LoadStoreFunct3.LH
            case "lw"   => LoadStoreFunct3.LW
            case "lbu"  => LoadStoreFunct3.LBU
            case "lhu"  => LoadStoreFunct3.LHU
        }

        // Define the opcode
        val opcode = Opcode.Load

        // Assemble the instruction
        BinaryInstructionFormats.iType(imm, rs1, funct3, rd, opcode)
    }

    def assembleStoreType(instr: String, rs2: Int, imm: Int, rs1: Int): Int = {

        // Define the funct3 field
        val funct3 = instr match {
            case "sb"   => LoadStoreFunct3.SB
            case "sh"   => LoadStoreFunct3.SH
            case "sw"   => LoadStoreFunct3.SW
        }

        // Define the opcode
        val opcode = Opcode.Store

        // Assemble the instruction
        BinaryInstructionFormats.sType(imm, rs2, rs1, funct3, opcode)
    }

    def assembleBranchType(instr: String, rs1: Int, rs2: Int, imm: Int): Int = {

        // Define the funct3 field
        val funct3 = instr match {
            case "beq"  => BranchFunct3.BEQ
            case "bne"  => BranchFunct3.BNE
            case "blt"  => BranchFunct3.BLT
            case "bge"  => BranchFunct3.BGE
            case "bltu" => BranchFunct3.BLTU
            case "bgeu" => BranchFunct3.BGEU
        }

        // Define the opcode
        val opcode = Opcode.Branch

        // Assemble the instruction
        BinaryInstructionFormats.bType(imm, rs2, rs1, funct3, opcode)
    }

    def assembleJumpType(instr: String, rd: Int, imm: Int): Int = {

        // Define opcode
        val opcode = Opcode.Jal

        // Assemble the instruction
        BinaryInstructionFormats.jType(imm, rd, opcode)
    }

    def assembleUpperType(instr: String, rd: Int, imm: Int): Int = {

        // Define opcode
        val opcode = instr match {
            case "lui"      => Opcode.Lui
            case "auipc"    => Opcode.AuiPc
        }

        // Assemble the instruction
        BinaryInstructionFormats.uType(imm, rd, opcode)
    }

    def assembleFenceType(instr: String, pred: Int, succ: Int): Int = {
        // Define opcode
        val opcode = Opcode.Fence

        // Assemble the instruction
        BinaryInstructionFormats.fenceType(pred, succ, opcode)
    }


    // Parsers
    def parseNumber(number: String): Int = {
        number match {
            case n if n.startsWith("0x")    => Integer.parseInt(n.substring(2), 16)
            case n if n.startsWith("x")     => Integer.parseInt(n.substring(1), 16)
            case n if n.startsWith("0b")    => Integer.parseInt(n.substring(2), 2)
            case n if n.startsWith("b")     => Integer.parseInt(n.substring(1), 2)
            case ""                         => 0
            case null                       => 0
            case _                          => number.toInt
        }
    }

    def parseAccessType(accessType: String): Int = {
        accessType.toUpperCase.foldLeft(0) {
            case (acc, 'W') => acc | 0b0001
            case (acc, 'R') => acc | 0b0010
            case (acc, 'O') => acc | 0b0100
            case (acc, 'I') => acc | 0b1000
            case (acc, _)   => acc
        }
    }

    def parseRegister(register: String): Int = {
        register.toLowerCase match {
            // x0-x31 notation
            case r if r.startsWith("x") =>
                val num = r.substring(1).toInt
                require(num >= 0 && num <= 31, s"Invalid register number: $num")
                num

            // ABI names

            case ""             => 0
            case "zero"         => 0
            case "ra"           => 1
            case "sp"           => 2
            case "gp"           => 3
            case "tp"           => 4
            case "t0"           => 5
            case "t1"           => 6
            case "t2"           => 7
            case "s0" | "fp"    => 8
            case "s1"           => 9
            case "a0"           => 10
            case "a1"           => 11
            case "a2"           => 12
            case "a3"           => 13
            case "a4"           => 14
            case "a5"           => 15
            case "a6"           => 16
            case "a7"           => 17
            case "s2"           => 18
            case "s3"           => 19
            case "s4"           => 20
            case "s5"           => 21
            case "s6"           => 22
            case "s7"           => 23
            case "s8"           => 24
            case "s9"           => 25
            case "s10"          => 26
            case "s11"          => 27
            case "t3"           => 28
            case "t4"           => 29
            case "t5"           => 30
            case "t6"           => 31
            case _ => throw new IllegalArgumentException(s"Unknown register: $register")
        }
    }

    def parseLabels(lines: Array[String]): List[(Option[String], Option[String])] = {
        val labelPattern = """^\s*([a-zA-Z_][a-zA-Z0-9_]*|\d+)\s*:\s*(.*)$""".r

        val parsed = lines.toList.map {
            case labelPattern(label, rest) =>
                val instruction = if (rest.trim.isEmpty) None else Some(rest.trim)
                (Some(label), instruction)
            case line =>
                (None, if (line.trim.isEmpty) None else Some(line.trim))
        }

        // Combine label-only lines with next instruction
        def combine(remaining: List[(Option[String], Option[String])]): List[(Option[String], Option[String])] = {
            remaining match {
                case Nil => Nil
                case (Some(label), None) :: (None, Some(inst)) :: tail =>
                    (Some(label), Some(inst)) :: combine(tail)
                case head :: tail =>
                    head :: combine(tail)
            }
        }

        combine(parsed)
    }




    def extractInstructions(labeledInstructions: List[(Option[String], Option[String])]): Array[String] = {
        labeledInstructions match {
            case Nil => Array.empty
            case (_, Some(instr)) :: tail =>
                Array(instr) ++ extractInstructions(tail)
            case (_, _) :: tail =>
                extractInstructions(tail)
        }
    }

    def loadAssembly(filepath: String): Array[Array[String]] = {
        val bufferedSource = Source.fromFile(filepath)
        val lines = bufferedSource.getLines()
          .map(line => line.split("#")(0).trim)  // Remove everything after #
          .filterNot(_.isEmpty)
          .toArray
        bufferedSource.close()

        val instructions = extractInstructions(parseLabels(lines))

        val asm = Array.fill(instructions.length, 4)("")

        for ((instruction, idx) <- instructions.zipWithIndex) {
            val parts  = instruction.split("""[\s,()]+""")
              .filter(_.nonEmpty)

            val instr = parts.headOption
              .getOrElse("")

            val fields = parts.drop(1).take(3)

            asm(idx)(0) = instr
            for ((field, jdx) <- fields.zipWithIndex) { asm(idx)(jdx + 1) = field }
        }
        asm
    }


    def assemble(filepath: String): Array[Int] = {
        val asm = loadAssembly(filepath)
        val binary = new Array[Int](asm.length)
        for ((instruction, idx) <- asm.zipWithIndex) {
            binary(idx) = instruction(0) match {
                case "add" | "sub" | "sll" | "slt" | "sltu" | "xor" | "srl" | "sra" | "or" | "and" =>
                    assembleRegisterType(instruction(0), parseRegister(instruction(1)), parseRegister(instruction(2)), parseRegister(instruction(3)))
                case "addi" | "slli" | "slti" | "sltiu" | "xori" | "srli" | "srai" | "ori" | "andi" | "jalr" | "ecall" | "ebreak" =>
                    assembleImmediateType(instruction(0), parseRegister(instruction(1)), parseRegister(instruction(2)), parseNumber(instruction(3)))
                case "lb" | "lh" | "lw" | "lbu" | "lhu" =>
                    assembleLoadType(instruction(0), parseRegister(instruction(1)), parseNumber(instruction(2)), parseRegister(instruction(3)))
                case "sb" | "sh" | "sw" =>
                    assembleStoreType(instruction(0), parseRegister(instruction(1)), parseNumber(instruction(2)), parseRegister(instruction(3)))
                case "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" =>
                    assembleBranchType(instruction(0), parseRegister(instruction(1)), parseRegister(instruction(2)), parseNumber(instruction(3)))
                case "jal" =>
                    assembleJumpType(instruction(0), parseRegister(instruction(1)), parseNumber(instruction(2)))
                case "lui" | "auipc" =>
                    assembleUpperType(instruction(0), parseRegister(instruction(1)), parseNumber(instruction(2)))
                case "fence" =>
                    assembleFenceType(instruction(0), parseAccessType(instruction(1)), parseAccessType(instruction(2)))
                case _ => 0
            }
        }
        binary
    }
}

//0000000000100 0001 001 0100 0 1100011
//0000000000100 0001 001 0100 0 1100011
//0000000000100 0001 000 0100 0 1100011

//class AssemblerTest(testFolderPath: String) {
//    private val path: Path = Paths.get(testFolderPath)
//    print(f"Testing files in: $path\n")
//    if (Files.exists(path) && Files.isDirectory(path)) {
//        val stream = Files.list(path) // Stream[Path]
//        try {
//            stream.iterator().asScala
//              .filter { file =>
//                  val fileName = file.getFileName.toString.toLowerCase
//                  fileName.endsWith(".s") || fileName.endsWith(".asm")
//              }
//              .foreach { file =>
//                print(f"Testing $file\n")
//                val asm = Files.readAllLines(file).asScala.toArray
//                print(asm.mkString("\n")+"\n")
//                val myAssembler = new Assembler()
//                val myBin = myAssembler.assemble(file.toString).map(int => f"$int%08x")
//
//
//                val tmpFile = s"$path/tmp_${file.getFileName}.o"
//                val cmd = Seq("riscv64-unknown-elf-as", file.toString, "-o", tmpFile) #|
//                  Seq("riscv64-unknown-elf-objdump", "-d", tmpFile) #|
//                  Seq("grep", "^ ") #|
//                  Seq("awk", "{print $2}")
//                val logger = ProcessLogger(_ => (), _ => ())
//                val refHex = cmd.!!(logger).split("\n")
//                //val refBin = refHex.map(hex => BigInt(hex, 16).toLong)
//
//                for ((asm, my, ref) <- asm.lazyZip(myBin).lazyZip(refHex).toArray) {
//                    if (my != ref) {
//                        print(f"Failed test file: $file\n")
//                        print(  "Instruction        myHex                   refHex\n")
//                        print(f"❌ $asm           ${my}               ${ref}\n")
//                        print(f"                             ${Integer.parseInt(my, 16).toBinaryString.reverse.padTo(32, '0').reverse}               ${Integer.parseInt(ref, 16).toBinaryString.reverse.padTo(32, '0').reverse}\n")
//                        throw new AssertionError(s"Mismatch in $file: $asm")
//                    }
//                }
//                print(f"✅ $file Succeeded\n\n")
//            }
//        } finally {
//            stream.close()
//        }
//    }
//
//    def toBin(n: Long): String = {
//        n.toInt.toBinaryString.reverse.padTo(32, '0').reverse
//    }
//
//    def toHex(n: Long): String = {
//        f"$n%08x"
//    }
//
//    def arrayToHex(array: Array[Long]): Unit = {
//        array.foreach(i =>
//            print(f"$i%08x\n"))
//    }
//}




//val as = new Assembler()
//
//toHex(as.assemble("/home/t/tests/test_arithmetic.s"))
//toHex(as.assemble("/home/t/tests/test_immediate.s"))
//toHex(as.assemble("/home/t/tests/test_load_store.s"))
//toHex(as.assemble("/home/t/tests/test_upper.s"))
//toHex(as.assemble("/home/t/tests/test_branch.s"))
//toHex(as.assemble("/home/t/tests/test_edge_cases.s"))
//toHex(as.assemble("/home/t/tests/test_jump.s"))
//toHex(as.assemble("/home/t/tests/test_system.s"))
//toHex(as.assemble("/home/t/tests/test_comprehensive.s"))



