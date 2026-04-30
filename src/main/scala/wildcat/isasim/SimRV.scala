/*
 * Copyright (c) 2015-2017, DTU
 * Simplified BSD License
 */

/*
 * A simple ISA simulator of RISC-V.
 * 
 * Author: Martin Schoeberl (martin@jopdesign.com)
 */

package wildcat.isasim

import net.fornwall.jelf.ElfFile
import utest.TestRunner
import wildcat.Opcode._
import wildcat.AluFunct3._
import wildcat.AluFunct7._
import wildcat.BranchFunct3._
import wildcat.LoadStoreFunct3._
import wildcat.CSRFunct3._
import wildcat.TagFunct3._
import wildcat.InstrType._
import wildcat.CSR._
import wildcat.{Util, dTag}
import wildcat.dTag._

class SimRV(mem: Array[Long], start: Long, stop: Long) {

  // That's the state of the processor.
  // That's it, nothing else (except memory ;-)
  var pc = start // RISC-V tests start at 0x200
  val reg = Array.fill(32)((i8, 0L))

  // Reservation state for LR/SC
  var reservationValid = false
  var reservationAddr = 0

  // stop on a test end
  var run = true;

  // some statistics
  var instrCnt = 0

  def execute(instr: Int, instr_length: Int): Boolean = {
    def genImm(opcode: Int) = {

      val instrType: InstrType = opcode match {
        case AluImm => I
        case Alu => R
        case Branch => SBT
        case Load => I
        case Store => S
        case Lui => U
        case AuiPc => U
        case Jal => UJ
        case JalR => I
        case System => I
        case Tag =>
          (instr >> 12) & 0x07 match {
            case STR => S
            case _ => I
          }
        case _ => R
      }
      // subfields of the instruction
      val instr7 = (instr >> 7) & 0x01
      val instr11_8 = (instr >> 8) & 0x0f
      val instr19_12 = (instr >> 12) & 0xff
      val instr20 = (instr >> 20) & 0x01
      val instr24_21 = (instr >> 21) & 0x0f
      val instr31_20 = (instr >> 20) & 0xfff
      val instr30_25 = (instr >> 25) & 0x3f
      val instr31 = (instr >> 31) & 0x01
      val sext8 = if (instr31 == 1) 0xff else 0
      val sext12 = if (instr31 == 1) 0xfff else 0

      // subfields of the immediate, depending on instruction type
      val imm0 = instrType match {
        case I => instr20
        case S => instr7
        case _ => 0
      }
      val imm4_1 = instrType match {
        case I => instr24_21
        case U => 0
        case UJ => instr24_21
        case _ => instr11_8
      }
      val imm10_5 = if (instrType == U) 0 else instr30_25
      val imm11 = instrType match {
        case SBT => instr7
        case U => 0
        case UJ => instr20
        case _ => instr31
      }
      val imm19_12 = if (instrType == U || instrType == UJ) instr19_12 else sext8
      val imm31_20 = if (instrType == U) instr31_20 else sext12

      // now glue together
      (imm31_20 << 20) | (imm19_12 << 12) | (imm11 << 11) |
        (imm10_5 << 5) | (imm4_1 << 1) | imm0
    }
    // Do some decoding: extraction of decoded fields
    val (opcode, rd, rs1, rs2, funct3, funct7, imm) : (Int, Int, Int, Int, Int, Int, Int) = if (instr_length == 4) {
      (instr & 0x7f,
      (instr >> 7) & 0x01f,
      (instr >> 15) & 0x01f,
      (instr >> 20) & 0x01f,
      (instr >> 12) & 0x07,
      (instr >> 25) & 0x07f, // Extended to 7 bits for AMO
      genImm(instr & 0x7f))
      //val aq = (instr >> 26) & 0x01 // Acquire bit
      //val rl = (instr >> 25) & 0x01 // Release bit
    } else {
      RVCdecompressor.expandRVC(instr)
    }
    /**
     * Immediate generation is a little bit elaborated,
     * but shall give smaller multiplexers in the hardware.
     */





    def explicitCast(old_tag: Int, value: Long, new_tag: Int): Long = {
      val old_tag_signedness = tagSignedness(old_tag)
      val new_tag_signedness = tagSignedness(new_tag)

      val old_tag_size = tagSize(old_tag)
      val new_tag_size = tagSize(new_tag)

      val old_tag_bit_width = tagSizeBitWidth(old_tag)
      val new_tag_bit_width = tagSizeBitWidth(new_tag)
      val old_tag_bit_mask  = tagSizeBitMask(old_tag)
      val new_tag_bit_mask  = tagSizeBitMask(new_tag)

      val downcast_sign_bit       = 1L << (new_tag_bit_width - 1)
      val upcast_sign_bit         = 1L << (old_tag_bit_width - 1)
      val downcast_extension_mask = ~new_tag_bit_mask
      val upcast_extension_mask   = ~old_tag_bit_mask
      val downcast_masked         = value & new_tag_bit_mask
      val upcast_masked           = value & old_tag_bit_mask

      // skip casting
      //if (old_tag == new_tag) {
      //  value
      //}

      // size downcasting OR same-size signcasting
      if (old_tag_size >= new_tag_size) {
        if (old_tag_signedness == 0) {
          downcast_masked
        }
        else {
          if ((downcast_masked & downcast_sign_bit) != 0) {
            downcast_masked | downcast_extension_mask
          } else {
            downcast_masked
          }
        }
      }
      // size upcasting
      else {
        if (old_tag_signedness == 0) {
          upcast_masked
        }
        else {
          if ((upcast_masked & upcast_sign_bit) != 0) {
            upcast_masked | upcast_extension_mask
          } else {
            upcast_masked
          }
        }
      }
    }

    def implicitCast(tag1: Int, val1: Long, tag2: Int, val2: Long) = {
      var common_tag: Int = 0
      val tag1_unsigned = isUnsigned(tag1)
      val tag2_unsigned = isUnsigned(tag2)

      if (tag1 == tag2) {
        common_tag = tag1
        (common_tag, val1, val2)
      }
      else {
        if (tag1 == NONE | tag2 == NONE) {
          common_tag = ~((~tag2) | (~tag1))
        }
        else if (tag1_unsigned == tag2_unsigned) {
          common_tag = Math.max(tag1, tag2)
        }
        else {
          val (signed_tag, unsigned_tag) = if (tag1_unsigned & !tag2_unsigned) (tag2, tag1) else (tag1, tag2)
          val (signed_tag_size, unsigned_tag_size) = (tagSize(signed_tag), tagSize(unsigned_tag))

          if (unsigned_tag_size >= signed_tag_size) {
            common_tag = unsigned_tag
          } else if (signed_tag_size > unsigned_tag_size) {
            common_tag = signed_tag
          }
        }

        val casted_val1 = explicitCast(tag1, val1, common_tag)
        val casted_val2 = explicitCast(tag2, val2, common_tag)

        (common_tag, casted_val1, casted_val2)
      }
    }

    def tagSize(tag: Int): Int = {
      (tag >> 1) & 0b11
    }
    def isUnsigned(tag: Int): Boolean = {
      tagSignedness(tag) == 0b0
    }
    def isSigned(tag: Int): Boolean = {
      !isUnsigned(tag)
    }
    def tagSignedness(tag: Int): Int = {
      (tag & 0b1)
    }
    def tagSizeBitWidth(tag: Int): Int = {
      math.pow(2,tagSize(tag)).toInt*8
    }
    def tagSizeBitMask(tag: Int): Long = {
      math.pow(2, tagSizeBitWidth(tag)).toLong - 1
    }
    def tagMutilpy(tag1: Int, tag2: Int, common_tag: Int): (Int, Int) = {
      val tag_sum = tagSizeBitWidth(tag1) + tagSizeBitWidth(tag2)
      val tag_signedness = tagSignedness(common_tag)

      val (lower_tag, upper_tag): (Int, Int) =
        if (tag_sum == 16) {
          (u16 | tag_signedness, NONE)
        } else if (tag_sum <= 32) {
          (u32 | tag_signedness, NONE)
        } else if (tag_sum <= 40) {
          (u32 | tag_signedness, u8 | tag_signedness)
        } else if (tag_sum <= 48) {
          (u32 | tag_signedness, u16 | tag_signedness)
        } else if (tag_sum <= 64) {
          (u32 | tag_signedness, u32 | tag_signedness)
        } else {
          (NONE, NONE)
        }

      (lower_tag, upper_tag)

    }
    def taggedRegisterLocator(displ: Int): (Int, Int, Int) = {
      //(tagRelativeWordAddr, tagByteOffset, valueRelativeAddr)
      (((displ >> 4) + ((displ >> 4) << 2)) << 2,  (displ >> 2) & 0b11, displ + 4 + ((displ >> 4) << 2))

      //tagSize(tag) match {
      //  case 0 => ((displ >> 2) << 1,                   (displ >> 0) & 0b11, displ + 4 + ((displ >> 2) << 2))
      //  case 1 => ((displ >> 3) + ((displ >> 3) << 1),  (displ >> 1) & 0b11, displ + 4 + ((displ >> 3) << 2))
      //  case 2 => ((displ >> 4) + ((displ >> 4) << 2),  (displ >> 2) & 0b11, displ + 4 + ((displ >> 4) << 2))
      //}
    }



    //val imm = genImm()


    // single bit on extended function - this is not nice
    val sraSub = funct7 == SRP_SUB && (opcode == Alu || (opcode == AluImm && funct3 == F3_SRL_SRA))

    def alu(funct3: Int, funct7: Int, sraSub: Boolean, op1: Long, tag1: Int, op2: Long, tag2: Int): (Int, Long) = {
      if ((tag1 & 0x1) != (tag2 & 0x1)  & debug) {
        throw new RuntimeException(s"both tags dont match in terms of signed/unsigned. tag1: $tag1 doesn't match tag2: $tag2")
      }

      val (common_tag, casted_op1, casted_op2): (Int, Long, Long) = implicitCast(tag1, op1, tag2, op2)
      val (lower_tag, upper_tag): (Int, Int) = tagMutilpy(tag1, tag2, common_tag)

      val shamt = math.abs(op2)// & (tagSizeBitWidth(tag1) - 1)

      val u64_mask = BigInt("FFFFFFFFFFFFFFFF", 16)
      //val tagd = math.max(tag1 & 0b110, tag2 & 0b110) | (tag1 & 0b1)
      (funct7, opcode) match {
        case (MUL_DIV, Alu) =>
          funct3 match {
            case F3_MUL =>
              if (tagSizeBitWidth(tag1) <= 32 & tagSizeBitWidth(tag2) <= 32) {
                (lower_tag, (((op1 * op2) & 0xffffffff) << 32) >> 32)
              } else {
                (lower_tag, op1 * op2)
              }
            case F3_MULH =>
              (isUnsigned(tag1), isUnsigned(tag2)) match {
                case (true, true)   => (upper_tag, (((BigInt(op1) & u64_mask) * (BigInt(op2) & u64_mask)) >> 64).toLong) //MULHU
                case (false, true)  => (upper_tag, ((BigInt(op1) * (BigInt(op2) & u64_mask)) >> 64).toLong) //MULHSU
                case (true, false)  => (upper_tag, ((BigInt(op2) * (BigInt(op1) & u64_mask)) >> 64).toLong) //MULHSU
                case (false, false) => (upper_tag, ((BigInt(op1) * BigInt(op2)) >> 64).toLong) //MULH
              }
            case F3_DIV =>
              if (casted_op2 == 0) {
                (common_tag, -1)
              }
              else if (casted_op1 == Long.MinValue & casted_op2 == -1 & isSigned(common_tag)) {
                (common_tag, Long.MinValue)
              }
              else {
                (isSigned(common_tag), tagSizeBitWidth(common_tag)) match {
                  case (true, 64)   => (common_tag, casted_op1 / casted_op2)                                                    //DIV
                  case (true, _)    => (common_tag, (casted_op1.toInt / casted_op2.toInt).toLong)                               //DIVW
                  case (false, 64)  => (common_tag, ((BigInt(casted_op1) & u64_mask) / (BigInt(casted_op2) & u64_mask)).toLong) //DIVU
                  case (false, _)   => (common_tag, (casted_op1 & 0xffffffffL) / (casted_op2 & 0xffffffffL))                    //DIVUW
                }
              }
            case F3_REM =>
              if (casted_op2 == 0) {
                (common_tag, casted_op1)
              }
              else if (casted_op1 == Long.MinValue & casted_op2 == -1 & isSigned(common_tag)) {
                (common_tag, 0)
              }
              else {
                (isSigned(common_tag), tagSizeBitWidth(common_tag)) match {
                  case (true, 64)   => (common_tag, casted_op1 % casted_op2)                                                    //REM
                  case (true, _)    => (common_tag, (casted_op1.toInt / casted_op2.toInt).toLong)                               //REMW
                  case (false, 64)  => (common_tag, ((BigInt(casted_op1) & u64_mask) % (BigInt(casted_op2) & u64_mask)).toLong) //REMU
                  case (false, _)   => (common_tag, (casted_op1 & 0xffffffffL) % (casted_op2 & 0xffffffffL))                    //REMUW
                }
              }
          }
        case (SRP_SUB,_) =>
          funct3 match {
            case F3_ADD_SUB => (common_tag, explicitCast(common_tag, casted_op1 - casted_op2, common_tag))
            case F3_SRL_SRA =>
              if (isSigned(tag1)) {
                (tag1, (op1 >> shamt) & tagSizeBitMask(tag1))
              } else {
                (tag1, (op1 >>> shamt) & tagSizeBitMask(tag1))
              }
          }
        case _ =>
          funct3 match {
            case F3_ADD_SUB => (common_tag, explicitCast(common_tag, casted_op1 + casted_op2, common_tag))
            case F3_SLL_SRP => (tag1, (op1 << shamt) & tagSizeBitMask(tag1))
            case F3_SRL_SRA =>
              if (isSigned(tag1)) {
                (tag1, (op1 >> shamt) & tagSizeBitMask(tag1))
              } else {
                (tag1, (op1 >>> shamt) & tagSizeBitMask(tag1))
              }
            case F3_SLT =>
              if (isSigned(common_tag)) {
                if (casted_op1 < casted_op2) (u8, 1) else (u8, 0)
              } else {
                if ((casted_op1 < casted_op2) ^ (casted_op1 < 0) ^ (casted_op2 < 0)) (u8, 1) else (u8, 0)
              }
            // output tag not inhereted to prevent any accidental leakage of information, unsigned byte choses as the smallest datatype that can represent 0 or 1
            case F3_XOR => (common_tag, casted_op1 ^ casted_op2)
            case F3_OR => (common_tag, casted_op1 | casted_op2)
            case F3_AND => (common_tag, casted_op1 & casted_op2)
          }
      }
    }

    def compare(funct3: Int, op1: Long, tag1: Int, op2: Long, tag2: Int): Boolean = {
      if ((tag1 & 0x1) != (tag2 & 0x1)  & debug) {
        throw new RuntimeException(s"both tags dont match in terms of signed/unsigned. tag1: $tag1 doesn't match tag2: $tag2")
      }

      val (common_tag, casted_op1, casted_op2): (Int, Long, Long) = implicitCast(tag1, op1, tag2, op2)

      funct3 match {
        case BEQ => casted_op1 == casted_op2
        case BNE => !(casted_op1 == casted_op2)
        case BLT =>
          if (isUnsigned(common_tag)) {
            (casted_op1 < casted_op2) ^ (casted_op1 < 0) ^ (casted_op2 < 0)
          } else {
            casted_op1 < casted_op2
          }
        case BGE =>
          if (isUnsigned(common_tag)) {
            casted_op1 == casted_op2 || ((casted_op1 > casted_op2) ^ (casted_op1 > 0) ^ (casted_op2 > 0))
          } else {
            casted_op1 >= casted_op2
          }
      }
    }

    def load(funct3: Int, base: Int, displ: Int): (Int, Long) = {
      val addr = (base + displ)
      val dword = mem(addr >>> 3)
      val byteOffset = addr & 0x07

      funct3 match {
        case LB  => (i8,  ((dword >> (8 * byteOffset)) << 56) >> 56)
        case LH  => (i16, ((dword >> (8 * (byteOffset & 0x06))) << 48) >> 48)
        case LW  => (i32, ((dword >> (8 * (byteOffset & 0x04))) << 32) >> 32)
        case LD  => (i64, dword)
        case LBU => (u8,  (dword >> (8 * byteOffset)) & 0xffL)
        case LHU => (u16, (dword >> (8 * (byteOffset & 0x06))) & 0xffffL)
        case LWU => (u32, (dword >> (8 * (byteOffset & 0x04))) & 0xffffffffL)
        case LDU => (u64, dword)
      }
    }

    def store(funct3: Int, base: Int, displ: Int, tag: Int, value: Int): Unit = {
      val addr = base + displ
      val wordAddr = addr >>> 2



      // Any store should invalidate reservations to the same address
      if (reservationValid && (addr >>> 2) == (reservationAddr >>> 2)) {
        reservationValid = false
      }

      funct3 match {
        case ST =>
          tag match {
            case `i8` | `u8` | `i16` | `u16` =>
              val shift = 8 * (addr & 0x03)
              val source_mask = tagSizeBitMask(tag)
              val destination_mask = ~(source_mask << shift)
              mem(wordAddr) = (mem(wordAddr) & destination_mask) | ((value & source_mask) << shift)
            case `i32` | `u32` =>
              if (addr == 0xf0000004) {
                println("out: " + value.toChar)
              } else {
                mem(wordAddr) = value
              }
          }
        //mem
        //case SB => {
        //  val mask = (addr & 0x03) match {
        //    case 0 => 0xffffff00
        //    case 1 => 0xffff00ff
        //    case 2 => 0xff00ffff
        //    case 3 => 0x00ffffff
        //  }
        //  mem(wordAddr) = (mem(wordAddr) & mask) | ((value & 0xff) << (8 * (addr & 0x03)))
        //}
        //case SH => {
        //  val mask = (addr & 0x03) match {
        //    case 0 => 0xffff0000
        //    case 2 => 0x0000ffff
        //  }
        //  mem(wordAddr) = (mem(wordAddr) & mask) | ((value & 0xffff) << (8 * (addr & 0x03)))
        //}
        //case SW => {
        //  // very primitive IO simulation
        //  if (addr == 0xf0000004) {
        //    println("out: " + value.toChar)
        //  } else {
        //    mem(wordAddr) = value
        //  }
        //}
      }
    }

    def ecall(): Int = {
      funct3 match {
        case ESYS => {
          run = false
          return 0
        }
        case CSRRS => {
          val v = imm & 0xfff match {
            case CYCLE => instrCnt // cycle
            case CYCLEH => 0 // cycleh
            case TIME => instrCnt // time
            case TIMEH => 0 // timeh
            case INSTRET => instrCnt // instret
            case INSTRETH => 0 // instreth

            case HARTID => 0 // hartid
            case MARCHID => WILDCAT_MARCHID

            case _ => 0 // this gets us around _start in the test cases
          }
          // println(s"csrrw ${imm & 0xfff} return: $v")
          return v
        }
        case _ => {
          println("Unknown ecall: " + funct3)
          return 0
        }
      }
    }

    def cast(funct3: Int, rs1: Long, rs1_tag: Int, new_tag: Int): (Int, Long) = {

      funct3 match {
        case CAST   => (new_tag, explicitCast(rs1_tag, rs1, new_tag))
        case CAST_T => (new_tag, rs1)
      }
    }

    def spillReload(funct3: Int, base: Int, displ: Int, tag: Int, value: Int): (Int, Long) = {
      val (tagRelativeWordAddr, tagByteOffset, valueRelativeAddr): (Int, Int, Int) = taggedRegisterLocator(displ)

      val wordBase = base >>> 2
      val tagWordAddr = (wordBase) + tagRelativeWordAddr
      val valueWordAddr = (base + valueRelativeAddr) >>> 2
      val shift = 8 * tagByteOffset

      funct3 match {
        case LTR =>
          val tag = ((mem(tagWordAddr) >> shift) & 0xff).toInt
          val value = mem(valueWordAddr)

          (tag, value)
        case STR =>
          val source_mask = tagSizeBitMask(u8)
          val destination_mask = ~(source_mask << shift)

          mem(tagWordAddr) = (mem(tagWordAddr) & destination_mask) | ((tag & source_mask) << shift)
          mem(valueWordAddr) = value
          (0, 0)
      }
    }

    //def atomic(funct5: Int, addr: Int, rs2Val: Int): (Int, Boolean) = {
    //  if ((addr & 0x3) != 0) {
    //    throw new Exception(f"Misaligned atomic address: 0x${addr}%08x")
    //  }
    //  val wordAddr = addr >>> 2
    //  val oldValue = mem(wordAddr)
    //
    //  funct5 match {
    //    case 0x02 => { // LR.W
    //      reservationValid = true
    //      reservationAddr = addr
    //      (oldValue, true)
    //    }
    //    case 0x03 => { // SC.W
    //      if (reservationValid && reservationAddr == addr) {
    //        mem(wordAddr) = rs2Val
    //        reservationValid = false
    //        (0, true) // Success: return 0
    //      } else {
    //        (1, true) // Failure: return non-zero
    //      }
    //    }
    //    case 0x01 => { // AMOSWAP.W
    //      mem(wordAddr) = rs2Val
    //      (oldValue, true)
    //    }
    //    case 0x00 => { // AMOADD.W
    //      val result = oldValue + rs2Val
    //      mem(wordAddr) = result
    //      (oldValue, true)
    //    }
    //    case 0x04 => { // AMOXOR.W
    //      val result = oldValue ^ rs2Val
    //      mem(wordAddr) = result
    //      (oldValue, true)
    //    }
    //    case 0x0C => { // AMOAND.W
    //      val result = oldValue & rs2Val
    //      mem(wordAddr) = result
    //      (oldValue, true)
    //    }
    //    case 0x08 => { // AMOOR.W
    //      val result = oldValue | rs2Val
    //      mem(wordAddr) = result
    //      (oldValue, true)
    //    }
    //    case _ => (0, false)
    //  }
    //}


    // read register tags and values
    val rs1Tag = reg(rs1)._1
    val rs2Tag = reg(rs2)._1

    val rs1Val = reg(rs1)._2
    val rs2Val = reg(rs2)._2


    // next pc
    val pcNext = pc + instr_length

    // Debug output for atomic instructions
    if (opcode == 0x2f) {
      println(f"Atomic instruction at pc=0x${pc}%08x: rs1=x${rs1}%d(0x${rs1Val}%08x) rs2=x${rs2}%d(0x${rs2Val}%08x) rd=x${rd}%d funct7=0x${funct7}%02x")
    }

    // Execute the instruction and return a tuple for the result:
    //   (ALU result, writeBack, next PC)
    val result: ((Int, Long), Boolean, Long) = opcode match {
      //case 0x2f => { // AMO - Atomic Memory Operations
      //  val addr = rs1Val
      //  if (funct3 != 0x2) {
      //    throw new Exception(f"Invalid funct3 for atomic operation: 0x${funct3}%x")
      //  }
      //  val funct5 = (funct7 >> 2) & 0x1f  // Get bits [31:27] for funct5
      //  val (value, success) = atomic(funct5, addr, rs2Val)
      //  (value, success, pcNext)
      //}
      case AluImm => (alu(funct3, funct7, sraSub, rs1Val, rs1Tag, imm, i16), true, pcNext)
      case Alu => (alu(funct3, funct7, sraSub, rs1Val, rs1Tag, rs2Val, rs2Tag), true, pcNext)
      case Branch => ((NONE, 0), false, if (compare(funct3, rs1Val, rs1Tag, rs2Val, rs2Tag)) pc + imm else pcNext)
      case Load => (load(funct3, rs1Val.toInt, imm), true, pcNext)
      case Store => store(funct3, rs1Val.toInt, imm, rs2Tag, rs2Val.toInt); ((NONE, 0), false, pcNext)
      case Lui => ((i32, imm), true, pcNext)
      case AuiPc => ((u32, pc + imm), true, pcNext)
      case Jal => ((u32, pc + 4), true, pc + imm)
      case JalR => ((u32, pc + 4), true, (rs1Val + imm) & 0xfffffffe)
      case Fence => ((NONE, 0), false, pcNext)
      case System => ((u32, ecall()), true, pcNext)
      case Tag =>
        funct3 match {
          case CAST | CAST_T => (cast(funct3, rs1Val, rs1Tag, imm), true, pcNext)
          case LTR => (spillReload(funct3,rs1Val.toInt, imm, NONE, 0), true, pcNext)
          case STR => (spillReload(funct3,rs1Val.toInt, imm, rs2Tag, rs2Val.toInt), false, pcNext)
        }

      case _ => throw new Exception("Opcode " + opcode + " at " + pc + " not (yet) implemented")
    }

    // External interference simulation (uncomment for testing)
    // if (scala.util.Random.nextInt(100) < 5) { // 5% chance
    //   reservationValid = false
    // }

    if (rd != 0 && result._2) {
      reg(rd) = (result._1._1,result._1._2)
    }

    val oldPc = pc
    pc = result._3

    instrCnt += 1

    pc != oldPc && run && pc < stop // detect endless loop or go beyond code to stop simulation
  }

  // ---- Compressed Instruction Logic -----

  def fetchByte(addr: Int): Int = {
    val dword = mem(addr >>> 3)
    ((dword >> ((addr & 0x07) * 8)) & 0xff).toInt
  }
  def fetchHalf(addr: Int): Int = {
    val b0 = fetchByte(addr)
    val b1 = fetchByte(addr + 1)
    ((b1 << 8) | b0).toInt
  }

  // enable to enforce strict type matching and disable implicit casting
  var debug = false
  var cont = true
  while (cont) {
    var instruction_32bit = 0
    var instruction_length = 0

    val first_16_bits = fetchHalf(pc.toInt)

    // 32-bit instruction
    if ((first_16_bits & 0x03) == 0x03) {
      instruction_length = 4

      val next_16_bits = fetchHalf((pc + 2).toInt)
      instruction_32bit = (next_16_bits << 16) | first_16_bits
    }
    // 16-bit instruction
    else {
        instruction_length = 2
        instruction_32bit = first_16_bits
    }

    cont = execute(instruction_32bit, instruction_length)

    print("\n")
    print("regs:  iTag    : HexValue  : DecimalValue\n")
    reg.zipWithIndex.foreach { case ((rTag, rVal), index) => printf("x%s:   %s %016x %d\n", index, iTag_to_string(rTag), rVal, rVal) }
    print("")
  }

  def iTag_to_string(iTag: Int): String = {
    iTag match {
      case dTag.u8 => "u8"
      case dTag.i8 => "i8"
      case dTag.u16 => "u16"
      case dTag.i16 => "i16"
      case dTag.u32 => "u32"
      case dTag.i32 => "i32"
      case dTag.u64 => "u64"
      case dTag.i64 => "i64"
    }
  }
  print("\n")
  print("regs:  iTag    : HexValue  : DecimalValue\n")
  reg.zipWithIndex.foreach { case ((rTag, rVal), index) => printf("x%s:   %s %016x %d\n", index, iTag_to_string(rTag), rVal, rVal) }

}

object SimRV {

  def runSimRV(file: String) = {
    val mem = new Array[Long](1024 * 256) // 1 MB, also check masking in load and store

    val (code, start) = Util.getCode(file)

    for (i <- 0 until code.length) {
      mem(i) = code(i)
    }

    val stop = start + code.length * 4

    // TODO: do we really want ot ba able to start at an arbitrary address?
    // Read in RV spec
    val sim = new SimRV(mem, start, stop)
    sim
  }

  def main(args: Array[String]): Unit = {
    runSimRV(args(0))
  }
}