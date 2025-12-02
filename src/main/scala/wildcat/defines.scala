package wildcat

object Opcode {
  val AluImm = 0x13
  val Alu = 0x33
  val Branch = 0x63
  val Load = 0x03
  val Store = 0x23
  val Lui = 0x37
  val AuiPc = 0x17
  val Jal = 0x6f
  val JalR = 0x67
  val Fence = 0x0f
  val System = 0x73
  val Tag = 0x0b
}

object dTag {
  val UNSIGNED_BYTE = 0b000
  val SIGNED_BYTE   = 0b001
  val UNSIGNED_HALF = 0b010
  val SIGNED_HALF   = 0b011
  val UNSIGNED_WORD = 0b100
  val SIGNED_WORD   = 0b101
  val NONE          = 0xff
}

object C_opcode {
  val C_ADDI4SPN, C_FLD, C_LW, C_ST = 0b00
  val C_J, C_JAL, C_BEQZ, C_BNEZ, C_LI, C_LUI, C_ADDI, C_ADDI16SP, C_SRPI, C_SRLI, C_SRAI, C_ANDI, C_AND, C_OR, C_XOR, C_SUB, C_NOP = 0b01
  val C_LWSP, C_STSP, C_JR, C_JALR, C_SLLI, C_MV, C_ADD, C_EBREAK, C_CAST = 0b10
}


object C_funct2 {
  val C_SUB = 0b00
  val C_XOR = 0b01
  val C_OR  = 0b10
  val C_AND = 0b11
}

object C_funct3 {
  val C_ADDI4SPN, C_NOP, C_ADDI, C_SLLI                                                               = 0b000
  val C_JAL                                                                                           = 0b001
  val C_LWSP, C_LW, C_LI                                                                              = 0b010
  val C_LUI, C_ADDI16SP                                                                               = 0b011
  val C_JR, C_MV, C_JALR, C_ADD, C_EBREAK, C_SRPI, C_SRLI, C_SRAI, C_ANDI, C_AND, C_OR, C_XOR, C_SUB  = 0b100
  val C_J                                                                                             = 0b101
  val C_BEQZ, C_STSP, C_ST                                                                            = 0b110
  val C_BNEZ, C_CAST                                                                                  = 0b111
}

object C_funct4 {
  val C_MV, C_JR  = 0b1000
  val C_ADD, C_JALR, C_EBREAK = 0b1001
}

object C_funct6 {
  val C_SUB, C_XOR, C_OR, C_AND = 0b100011
}




object C_funct {
  val C_ADDI4SPN, C_NOP, C_ADDI, C_SLLI = 0b000
  val C_JAL                             = 0b001
  val C_LWSP, C_LW, C_LI                = 0b010
  val C_LUI, C_ADDI16SPN                = 0b011
  //val e                                 = 0b100
  val C_J                               = 0b101
  val C_BEQZ, C_SWSP, C_SW              = 0b110
  val C_BNEZ                            = 0b111
  val C_JR, C_MV                        = 0b1000
  val C_JALR, C_ADD, C_EBREAK           = 0b1001
  val C_SRLI                            = 0b10000
  val C_SRAI                            = 0b10001
  val C_ANDI                            = 0b10010
  val C_AND                             = 0b10001111
  val C_OR                              = 0b10001110
  val C_XOR                             = 0b10001101
  val C_SUB                             = 0b10001100
}

object InstrType extends Enumeration {
  type InstrType = Value
  val R, I, S, SBT, U, UJ = Value
}

object AluType extends Enumeration {
  type AluType = Value
  val ADD, SUB, SLL, SLT, SLTU, XOR, SRP, SRL, SRA, OR, AND = Value
}

object AluFunct7 {
  val DEFAULT = 0x00
  val SRP_SUB = 0x20

  //RV32M
  val MUL_DIV = 0x01
}

object AluFunct3 {
  val F3_ADD_SUB = 0x00 // no SUB in I-type
  val F3_SLL_SRP = 0x01
  val F3_SLT = 0x02
  val F3_SLTU = 0x03
  val F3_XOR = 0x04
  val F3_SRL_SRA = 0x05
  val F3_OR = 0x06
  val F3_AND = 0x07

  //RV32M
  val F3_MUL = 0x00
  val F3_MULH = 0x01
  val F3_MULHSU = 0x02
  val F3_MULHU = 0x03
  val F3_DIV = 0x04
  val F3_DIVU = 0x05
  val F3_REM = 0x06
  val F3_REMU = 0x07
}

object BranchFunct3 {
  val BEQ = 0x00
  val BNE = 0x01
  val BLT = 0x04
  val BGE = 0x05
  val BLTU = 0x06
  val BGEU = 0x07
}

object LoadStoreFunct3 {
  val LB = 0x00
  val LH = 0x01
  val LW = 0x02
  val LBU = 0x04
  val LHU = 0x05
  val LWU = 0x06
  val SB, ST = 0x00
  val SH = 0x01
  val SW = 0x02
}

object TagFunct3 {
  val CAST = 0x00
  val CAST_T = 0x01
  val LTR = 0x02
  val STR = 0x03
}

object CSRFunct3 {
  val ESYS = 0x00
  val CSRRW = 0x01
  val CSRRS = 0x02
  val CSRRC = 0x03
  val CSRRWI = 0x05
  val CSRRSI = 0x06
  val CSRRCI = 0x07
}

object CSR {
  val CYCLE = 0xc00
  val CYCLEH = 0xc80
  val TIME = 0xc01
  val TIMEH = 0xc81
  val MCYCLE = 0xb00
  val MCYCLEH = 0xb80
  // Disassembler does not know them
  val MTIME = 0xb01
  val MTIMEH = 0xb81

  val INSTRET = 0xc02
  val INSTRETH = 0xc82


  val HARTID = 0xf10
  val MARCHID = 0xf12
  val WILDCAT_MARCHID = 47 // see https://github.com/riscv/riscv-isa-manual/blob/main/marchid.md

  val MINSTRET = 0xb02
  val MINSTRETH = 0xb82
}