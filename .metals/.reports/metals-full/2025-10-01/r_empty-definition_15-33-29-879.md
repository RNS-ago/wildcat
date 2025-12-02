error id: file://<WORKSPACE>/src/main/scala/wildcat/assembler.scala:`<none>`.
file://<WORKSPACE>/src/main/scala/wildcat/assembler.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 559
uri: file://<WORKSPACE>/src/main/scala/wildcat/assembler.scala
text:

```scala
import wildcat.Opcode
import wildcat.AluFunct3
import wildcat.AluFunct7
import wildcat.BranchFunct3._
import wildcat.LoadStoreFunct3._
import wildcat.CSRFunct3._
import wildcat.InstrType._
import wildcat.CSR._


def assembleRegisterType(instr: String, rd: Int, rs1: Int, rs2: Int): Int = {

  // Define the funct7 field
  val funct7 = instr match {
    case "sub" | "sra" => AluFunct7.SRP_SUB
    case _ => AluFunct7.DEFAULT
  }

  // Define the funct3 field
  val funct3 = instr match {
    case "add" | "sub" => AluFunct3.F3_ADD_@@ SUB
    case "sll" => AluFunct3.F3_SLL_SRP
    case "slt" => AluFunct3.F3_SLT
    case "sltu" => AluFunct3.F3_SLTU
    case "xor" => AluFunct3.F3_XOR
    case "srl" | "sra" => AluFunct3.F3_SRL_SRA
    case "or" => AluFunct3.F3_OR
    case "and" => AluFunct3.F3_AND
  }

  val opcode = Opcode.Alu

  val instructionBinary =
    funct7 << 25 |
      rs2 << 20 |
      rs1 << 15 |
      rd << 7 |
      opcode << 0

  return instructionBinary
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.