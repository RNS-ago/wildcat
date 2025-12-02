package wildcat.isasim

import wildcat.{C_funct3, AluFunct3, TagFunct3, AluFunct7}
import wildcat.Opcode
import wildcat.LoadStoreFunct3


object RVCdecompressor {

  def decompressRegister(reg: Int) : Int = {
    reg match {
      case 0b000 => 8
      case 0b001 => 9
      case 0b010 => 10
      case 0b011 => 11
      case 0b100 => 12
      case 0b101 => 13
      case 0b110 => 14
      case 0b111 => 15
    }
  }

  def expandRVC(instr: Int): (Int, Int, Int, Int, Int, Int, Int) = {
    val opcode = instr & 0x3  // Fixed: should be 0x3 to get bits [1:0]
    val func3 = (instr >> 13) & 0b111
    val func4 = (instr >> 12) & 0b1111
    val sp = 2
    val ra = 1

    val imm2_6 = (instr>>2) & 0b11111
    val imm2_12 = (instr>>2) & 0b11111111111
    val imm5_6 = (instr>>5) & 0b11
    val imm5_12 = (instr>>5) & 0b11111111
    val imm7_12 = (instr>>7) & 0b111111
    val imm10_11 = (instr>>10) & 0b11
    val imm10_12 = (instr>>10) & 0b111
    val imm12 = (instr>>12) & 0b1

    // JAL/J offset calculation (CJ-format)
    val offset_j = ((imm12 << 31) >> 20) |  // Sign extend bit 12 to bit 11
      ((imm2_12 & 0b10000000000) >> 4) |  // bit 11 -> bit 7
      ((imm2_12 & 0b01100000000) >> 7) |  // bits 10:9 -> bits 3:2
      ((imm2_12 & 0b00010000000) >> 2) |  // bit 8 -> bit 6
      ((imm2_12 & 0b00001000000) << 1) |  // bit 7 -> bit 8
      ((imm2_12 & 0b00000100000) >> 1) |  // bit 6 -> bit 5
      ((imm2_12 & 0b00000011110) << 7) |  // bits 5:2 -> bits 11:8
      ((imm2_12 & 0b00000000001) << 4)    // bit 1 -> bit 4

    // Branch offset calculation (CB-format)
    val offset_b = ((imm12 << 31) >> 23) |  // Sign extend bit 12 to bit 8
      ((imm10_12 & 0b110) << 3) |  // bits 11:10 -> bits 5:4
      ((imm5_6 & 0b11) << 6) |     // bits 6:5 -> bits 7:6
      ((imm2_6 & 0b11000) >> 2) |  // bits 4:3 -> bits 2:1
      ((imm2_6 & 0b00111) << 1) |  // bits 2:0 -> bits 3:1
      ((imm10_12 & 0b001) << 5)    // bit 9 -> bit 5

    val rd_rs1 = (instr>>7) & 0b11111
    val rs2 = (instr>>2) & 0b11111

    val rd_rs2_3 = decompressRegister((instr>>2) & 0b111)
    val rd_rs1_3 = decompressRegister((instr>>7) & 0b111)


    opcode match {
      case 0b00 =>
        func3 match {
          case C_funct3.C_ADDI4SPN  => (Opcode.AluImm,  rd_rs2_3, sp,       0,        0,                  0,  imm5_12 << 2                                                  )
          case C_funct3.C_ST        => (Opcode.Store,   0,        rd_rs1_3, rd_rs2_3, LoadStoreFunct3.ST, 0,  ((imm5_6 & 0b1) << 6) | (imm10_12 << 3) | ((imm5_6 & 0b10) << 1))
          case C_funct3.C_LW        => (Opcode.Load,    rd_rs2_3, rd_rs1_3, 0,        LoadStoreFunct3.LW, 0,  ((imm5_6 & 0b1) << 6) | (imm10_12 << 3) | ((imm5_6 & 0b10) << 1))
          case _                    => (0, 0, 0, 0, 0, 0, 0)  // Illegal instruction
        }
      case 0b01 =>
        func3 match {
          case C_funct3.C_ADDI                                        =>  (Opcode.AluImm, rd_rs1, rd_rs1, 0, 0, 0, ((imm12 << 31) >> 26) | imm2_6)
          case C_funct3.C_JAL                                         =>  (Opcode.Jal, ra, 0, 0, 0, 0, offset_j)
          case C_funct3.C_LI                                          =>  (Opcode.AluImm, rd_rs1, 0, 0, 0, 0, ((imm12 << 31) >> 26) | imm2_6)
          case C_funct3.C_LUI | C_funct3.C_ADDI16SP                   =>
            if (rd_rs1 == 2) {
              // C.ADDI16SP
              val addi16sp_imm = ((imm12 << 31) >> 22) |
                ((imm2_6 & 0b10000) << 5) |   // bit 6 -> bit 9
                ((imm2_6 & 0b01000) << 3) |   // bit 5 -> bit 8
                ((imm2_6 & 0b00110) << 4) |   // bits 4:3 -> bits 7:6
                ((imm2_6 & 0b00001) << 5)     // bit 2 -> bit 5
              (Opcode.AluImm, sp, sp, 0, 0, 0, addi16sp_imm)
            } else if (rd_rs1 != 0) {
              // C.LUI
              (Opcode.Lui, rd_rs1, 0, 0, 0, 0, ((imm12 << 31) >> 14) | (imm2_6 << 12))
            } else {
              (0, 0, 0, 0, 0, 0, 0)  // Illegal if rd=0
            }
          case C_funct3.C_SRLI | C_funct3.C_SRAI |
               C_funct3.C_ANDI | C_funct3.C_AND |
               C_funct3.C_OR | C_funct3.C_XOR | C_funct3.C_SUB        =>
            imm10_11 match {
              case 0b00 => (Opcode.AluImm, rd_rs1_3, rd_rs1_3, 0, AluFunct3.F3_SLL_SRP, AluFunct7.SRP_SUB, ((imm12 << 31) >> 26) | imm2_6)  // C.SRP
              //case 0b01 => (Opcode.AluImm, rd_rs1_3, rd_rs1_3, 0, AluFunct3.F3_SLL_SRP, 0x20, ((imm12 << 31) >> 26) | imm2_6)  // C.SRAI
              case 0b10 => (Opcode.AluImm, rd_rs1_3, rd_rs1_3, 0, 7, 0, ((imm12 << 31) >> 26) | imm2_6)  // C.ANDI
              case 0b11 =>
                imm12 match {
                  case 0 =>
                    imm2_6 match {
                      case 0b00000 => (Opcode.Alu, rd_rs1_3, rd_rs1_3, rd_rs2_3, 0, 0x20, 0)  // C.SUB
                      case 0b00001 => (Opcode.Alu, rd_rs1_3, rd_rs1_3, rd_rs2_3, 4, 0, 0)     // C.XOR
                      case 0b00010 => (Opcode.Alu, rd_rs1_3, rd_rs1_3, rd_rs2_3, 6, 0, 0)     // C.OR
                      case 0b00011 => (Opcode.Alu, rd_rs1_3, rd_rs1_3, rd_rs2_3, 7, 0, 0)     // C.AND
                      case _       => (0, 0, 0, 0, 0, 0, 0)  // Reserved/Illegal
                    }
                  case _ => (0, 0, 0, 0, 0, 0, 0)  // Reserved for RV64/RV128
                }
            }
          case C_funct3.C_J                                           => (Opcode.Jal, 0, 0, 0, 0, 0, offset_j)  // C.J - JAL with rd=x0
          case C_funct3.C_BEQZ                                        => (Opcode.Branch, 0, rd_rs1_3, 0, 0, 0, offset_b)  // BEQ rs1', x0, offset
          case C_funct3.C_BNEZ                                        => (Opcode.Branch, 0, rd_rs1_3, 0, 1, 0, offset_b)  // BNE rs1', x0, offset
          case _                                                      => (0, 0, 0, 0, 0, 0, 0)  // Illegal instruction
        }
      case 0b10 =>
        func3 match {
          case C_funct3.C_CAST                                        => (Opcode.Tag, rd_rs1, rd_rs1, 0, TagFunct3.CAST, 0, ((imm12 << 31) >> 26) | imm2_6)  // C.SLLI
          case C_funct3.C_SLLI                                        => (Opcode.AluImm, rd_rs1, rd_rs1, 0, AluFunct3.F3_SLL_SRP, 0, ((imm12 << 31) >> 26) | imm2_6)  // C.SLLI
          case C_funct3.C_LWSP                                        =>
            if (rd_rs1 != 0) {
              val ltrsp_imm = ((imm12 & 0b1) << 5) | (imm2_6 & 0b11100) | ((imm2_6 & 0b11) << 6)
              (Opcode.Load, rd_rs1, sp, 0, LoadStoreFunct3.LW, 0, ltrsp_imm)
            } else {
              (0, 0, 0, 0, 0, 0, 0)  // Illegal if rd=0
            }
          case C_funct3.C_JR | C_funct3.C_MV | C_funct3.C_JALR | C_funct3.C_ADD =>
            if (imm12 == 0) {
              // C.JR or C.MV
              if (rs2 == 0) {
                // C.JR - JALR x0, 0(rs1)
                if (rd_rs1 != 0) {
                  (Opcode.JalR, 0, rd_rs1, 0, 0, 0, 0)
                } else {
                  (0, 0, 0, 0, 0, 0, 0)  // Illegal if rs1=0
                }
              } else {
                // C.MV - ADD rd, x0, rs2
                (Opcode.Alu, rd_rs1, 0, rs2, 0, 0, 0)
              }
            } else {
              // C.JALR or C.ADD
              if (rs2 == 0) {
                // C.JALR - JALR x1, 0(rs1)
                if (rd_rs1 != 0) {
                  (Opcode.JalR, ra, rd_rs1, 0, 0, 0, 0)
                } else {
                  // C.EBREAK
                  (Opcode.System, 0, 0, 0, 0, 0, 1)
                }
              } else {
                // C.ADD - ADD rd, rd, rs2
                (Opcode.Alu, rd_rs1, rd_rs1, rs2, 0, 0, 0)
              }
            }
          case C_funct3.C_STSP                                        =>
            val strsp_imm = ((imm7_12 & 0b111100) >> 2) | (imm7_12 & 0b000011)
            (Opcode.Store, 0, sp, rs2, LoadStoreFunct3.ST, 0, strsp_imm << 2)
          case _                                                      => (0, 0, 0, 0, 0, 0, 0)  // Illegal instruction
        }
      case _ => (0, 0, 0, 0, 0, 0, 0)  // Illegal instruction (quadrant 3 is for 32-bit instructions)
    }
  }

}