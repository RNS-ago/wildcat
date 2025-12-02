error id: file://<WORKSPACE>/src/main/scala/wildcat/assembler.scala:filterNot.
file://<WORKSPACE>/src/main/scala/wildcat/assembler.scala
empty definition using pc, found symbol in pc: filterNot.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -wildcat/BranchFunct3.asm.filterNot.
	 -wildcat/BranchFunct3.asm.filterNot#
	 -wildcat/BranchFunct3.asm.filterNot().
	 -wildcat/CSRFunct3.asm.filterNot.
	 -wildcat/CSRFunct3.asm.filterNot#
	 -wildcat/CSRFunct3.asm.filterNot().
	 -wildcat/InstrType.asm.filterNot.
	 -wildcat/InstrType.asm.filterNot#
	 -wildcat/InstrType.asm.filterNot().
	 -wildcat/CSR.asm.filterNot.
	 -wildcat/CSR.asm.filterNot#
	 -wildcat/CSR.asm.filterNot().
	 -asm/filterNot.
	 -asm/filterNot#
	 -asm/filterNot().
	 -scala/Predef.asm.filterNot.
	 -scala/Predef.asm.filterNot#
	 -scala/Predef.asm.filterNot().
offset: 3826
uri: file://<WORKSPACE>/src/main/scala/wildcat/assembler.scala
text:

```scala
package wildcat

import scala.io.Source

import wildcat.Opcode
import wildcat.AluFunct3
import wildcat.AluFunct7
import wildcat.BranchFunct3._
import wildcat.LoadStoreFunct3
import wildcat.CSRFunct3._
import wildcat.InstrType._
import wildcat.CSR._


class Assembler {

  def assembleRegisterType(instr: String, rd: Int, rs1: Int, rs2: Int): Int = {

    // Define the funct7 field
    val funct7 = instr match {
      case "sub" | "sra" => AluFunct7.SRP_SUB
      case _ => AluFunct7.DEFAULT
    }

    // Define the funct3 field
    val funct3 = instr match {
      case "add" | "sub" => AluFunct3.F3_ADD_SUB
      case "sll" => AluFunct3.F3_SLL_SRP
      case "slt" => AluFunct3.F3_SLT
      case "sltu" => AluFunct3.F3_SLTU
      case "xor" => AluFunct3.F3_XOR
      case "srl" | "sra" => AluFunct3.F3_SRL_SRA
      case "or" => AluFunct3.F3_OR
      case "and" => AluFunct3.F3_AND
    }

    // Define the opcode
    val opcode = Opcode.Alu

    // Assemble the instruction
    val instructionBinary =
      funct7 << 25 |
        rs2 << 20 |
        rs1 << 15 |
        rd << 7 |
        opcode << 0

    return instructionBinary
  }

  def assembleImmediateType(instr: String, rd: Int, rs1: Int, imm: Int): Int = {

    // Define the funct3 field
    val funct3 = instr match {
      case "addi" => AluFunct3.F3_ADD_SUB
      case "slli" => AluFunct3.F3_SLL_SRP
      case "slti" => AluFunct3.F3_SLT
      case "sltiu" => AluFunct3.F3_SLTU
      case "xori" => AluFunct3.F3_XOR
      case "srli" | "srai" => AluFunct3.F3_SRL_SRA
      case "ori" => AluFunct3.F3_OR
      case "andi" => AluFunct3.F3_AND
    }

    // Define the opcode
    val opcode = Opcode.AluImm

    // Assemble the instruction
    val instructionBinary =
      imm << 20 |
        rs1 << 15 |
        funct3 << 12 |
        rd << 7 |
        opcode << 0

    return instructionBinary
  }

  def assembleLoadType(instr: String, rd: Int, rs1: Int, imm: Int): Int = {

    // Define the funct3 field
    val funct3 = instr match {
      case "lb" => LoadStoreFunct3.LB
      case "lh" => LoadStoreFunct3.LH
      case "lw" => LoadStoreFunct3.LW
      case "lbu" => LoadStoreFunct3.LBU
      case "lhu" => LoadStoreFunct3.LHU
    }

    // Define the opcode
    val opcode = Opcode.Load

    // Assemble the instruction
    val instructionBinary =
      imm << 20 |
        rs1 << 15 |
        funct3 << 12 |
        rd << 7 |
        opcode << 0

    return instructionBinary
  }

  //def assembleStoreType(instr: String, rs1: Int, rs2: Int, imm: Int): Int = {}


  def loadAssembly(filepath: String): Array[Array[String]] = {
    val lines = Source.fromFile(filepath).getLines()
      .filterNot(_.trim.isEmpty)
      .toArray
    val asm = new Array[Array[String]](lines.length)

    for (i <- 0 until lines.length) {

      val instr = lines(i).split(" ")
        .head
        .filterNot(_.isWhitespace)
      val fields = lines(i).split(" ")
        .tail
        .mkString("").
        filterNot(_.isWhitespace).
        split("[,()]")

      asm(i) = instr +: fields
    }
    asm.f @@ ilterNot(_.trim.isEmpty)
      .toArray
  }

  //def assemble(filepath: String): Array[Int] = {
  //    val asm = loadAssembly(filepath)
  //    var binary = new Array[Int](asm.length)
  //    for (i <- 0 until asm.length) {
  //
  //        //binary(i) = asm(i)(0) match {
  //        //    case "add" | "sub" | "sll" | "slt" | "sltu" | "xor" | "srl" | "sra" | "or" | "and" => assembleRegisterType(asm(i)(0), asm(i)(1).toInt, asm(i)(2).toInt, asm(i)(3).toInt)
  //        //    case "addi" | "slli" | "slti" | "sltiu" | "xori" | "srli" | "srai" | "ori" | "andi" => assembleImmediateType(asm(i)(0), asm(i)(1).toInt, asm(i)(2).toInt, asm(i)(3).toInt)
  //        //    case "lb" | "lh" | "lw" | "lbu" | "lhu" => assembleLoadType(asm(i)(0), asm(i)(1).toInt, asm(i)(2).toInt, asm(i)(3).toInt)
  //        //
  //        
  //    }
  //}
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: filterNot.