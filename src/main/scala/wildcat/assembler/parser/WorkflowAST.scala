package wildcat.assembler.parser

import wildcat.{C_opcode, Opcode}
import wildcat.Opcode._

import scala.util.parsing.input.Positional

sealed trait WorkflowAST extends Positional
case class AndThen(step1: WorkflowAST, step2: WorkflowAST) extends WorkflowAST
case class ReadInput(inputs: Seq[String]) extends WorkflowAST
case class CallService(serviceName: String) extends WorkflowAST
case class Choice(alternatives: Seq[ConditionThen]) extends WorkflowAST
case object Exit extends WorkflowAST

sealed trait ConditionThen extends Positional { def thenBlock: WorkflowAST }
case class IfThen(predicate: Condition, thenBlock: WorkflowAST) extends ConditionThen
case class OtherwiseThen(thenBlock: WorkflowAST) extends ConditionThen

sealed trait Condition extends Positional
case class Equals(factName: String, factValue: String) extends Condition


sealed trait Target extends Positional
case class TSymbol(name: String) extends Target               // e.g., foo
case class TLocal(n: Int, forward: Boolean) extends Target    // e.g., 1f / 2b
case class TImm(bytes: Int) extends Target                    // rare for branches; allow anyway

case class LabelDef(name: String) extends InstructionAST { val opcode = -1 } // or separate stream
case class LocalLabelDef(n: Int) extends InstructionAST { val opcode = -1 }





sealed trait InstructionAST extends Positional {
  def opcode: Int
}


case class RTypeAST(
                          opcode: Int = Alu,
                          rd: Int,
                          funct3: Int,
                          rs1: Int,
                          rs2: Int,
                          funct7: Int
                        ) extends InstructionAST


sealed trait ITypeAST extends InstructionAST{
  def rd: Int
  def funct3: Int
  def rs1: Int
  def imm: Int
}
case class ImmArithAST(
                            opcode: Int = AluImm,
                            rd: Int,
                            funct3: Int,
                            rs1: Int,
                            imm: Int
                          ) extends ITypeAST
case class LoadAST(
                            opcode: Int = Load,
                            rd: Int,
                            funct3: Int,
                            rs1: Int,
                            imm: Int
                          ) extends ITypeAST
case class JumpLinkRegAST(
                            opcode: Int = JalR,
                            rd: Int,
                            funct3: Int = 0x0,
                            rs1: Int,
                            imm: Int
                          ) extends ITypeAST
case class SystemAST(
                            opcode: Int = System,
                            rd: Int = 0x0,
                            funct3: Int = 0x0,
                            rs1: Int = 0x0,
                            imm: Int
                          ) extends ITypeAST

case class CastAST(
                      opcode: Int = Tag,
                      rd: Int,
                      funct3: Int,
                      rs1: Int,
                      imm: Int
                    ) extends ITypeAST


case class BTypeAST(
                     opcode: Int = Branch,
                     funct3: Int,
                     rs1: Int,
                     rs2: Int,
                     target: Target
                   ) extends InstructionAST

case class JTypeAST(
                     opcode: Int = Jal,
                     rd: Int,
                     target: Target
                   ) extends InstructionAST

case class STypeAST(
                     opcode: Int = Store,
                     funct3: Int,
                     rs1: Int,
                     rs2: Int,
                     offset: Int
                   ) extends InstructionAST



sealed trait UTypeAST extends InstructionAST {
  def rd: Int
  def imm: Int
}
case class LoadUpperImmAST(
                            opcode: Int = Lui,
                            rd: Int,
                            imm: Int
                          ) extends UTypeAST
case class AddUpperImmAST(
                            opcode: Int = AuiPc,
                            rd: Int,
                            imm: Int
                          ) extends UTypeAST



case class FenceTypeAST(
                          opcode: Int = Fence,
                          rd: Int = 0x0,
                          funct3: Int = 0x0,
                          rs1: Int = 0x0,
                          succ: Int,
                          pred: Int,
                          fm: Int = 0x0
                        ) extends InstructionAST



// Compressed instructions

sealed trait CompressedAST extends InstructionAST

case class CRTypeAST(
                      opcode: Int = 0b10,
                      funct4: Int,
                      rd_rs1: Int,
                      rs2: Int,
                    ) extends CompressedAST

case class CITypeAST(
                      opcode: Int,
                      funct3: Int,
                      rd_rs1: Int,
                      imm12: Int,
                      imm2_6: Int,
                    ) extends CompressedAST

case class CSSTypeAST(
                      opcode: Int,
                      funct3: Int,
                      rs2: Int,
                      imm7_12: Int
                    ) extends CompressedAST

case class CIWTypeAST(
                       opcode: Int = 0b00,
                       funct3: Int,
                       rd: Int,
                       imm5_12: Int
                     ) extends CompressedAST

case class CLTypeAST(
                       opcode: Int = 0b10,
                       funct3: Int,
                       rs1: Int,
                       rd: Int,
                       imm5_6: Int,
                       imm10_12: Int
                     ) extends CompressedAST

case class CSTypeAST(
                       opcode: Int = 0b10,
                       funct3: Int,
                       rs1: Int,
                       rs2: Int,
                       imm5_6: Int,
                       imm10_12: Int
                     ) extends CompressedAST

case class CATypeAST(
                      opcode: Int,
                      funct6: Int,
                      rd_rs1: Int,
                      funct2: Int,
                      rs2: Int
                    ) extends CompressedAST


sealed trait CBTypeAST extends CompressedAST{
  def opcode: Int
  def funct3: Int
  def rd_rs1: Int
}
case class CBArithmTypeAST(
                           opcode: Int,
                           funct3: Int,
                           rd_rs1: Int,
                           imm2_6: Int,
                           imm10_12: Int
                         ) extends CBTypeAST
case class CBranchTypeAST(
                         opcode: Int,
                         funct3: Int,
                         rd_rs1: Int,
                         target: Target
                         ) extends CBTypeAST

case class CJTypeAST(
                      opcode: Int,
                      funct3: Int,
                      target: Target
                     ) extends CompressedAST



