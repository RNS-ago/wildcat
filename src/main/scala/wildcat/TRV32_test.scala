package wildcat

import wildcat.assembler.compiler.{WorkflowCompiler, WorkflowLexerError, WorkflowParserError}
import wildcat.assembler.compiler.WorkflowCompiler.compile
import wildcat.assembler.lexer.WorkflowLexer
import wildcat.assembler.parser.WorkflowParser
import wildcat.isasim.SimRV

object TRV32_test {

  private val srp_test =
    """# Signed Right Shift
      |addi x2, x2, 3
      |addi x3, x3, -1
      |srp x4, x3, x2
      |# Unsigned Right Shift
      |cast x5, x3, u_word
      |srp x5, x5, x2
      |""".stripMargin

  private val srpi_test =
    """# Signed Right Shift by Immediate
      |addi x3, x3, -1
      |srpi x4, x3, 3
      |# Unsigned Right Shift by Immediate
      |cast x5, x3, u_word
      |srpi x5, x5, 3
      |""".stripMargin

  private val slt_test =
    """# Signed - Signed SLT
      |addi x3, x3, -1
      |addi x4, x4, 1
      |slt x10, x3, x4
      |# Unsigned - Signed SLT
      |cast x5, x3, u_word
      |slt x11, x5, x4
      |# Signed - Unsigned SLT
      |slt x12, x4, x5
      |# Unsigned - Unsigned SLT
      |cast x5, x3, u_word
      |cast x6, x4, u_word
      |slt x13, x5, x6
      |""".stripMargin

  private val slti_test =
    """# Signed - Signed SLT
      |addi x3, x3, -1
      |slti x10, x3, 1
      |# Unsigned - Signed SLT
      |cast x3, x3, u_word
      |slti x11, x3, 1
      |""".stripMargin

  private val load_store_test =
    """# Init Stack pointer and registers
      |lui x2, 0x1
      |addi x3, x3, -1
      |
      |# Byte
      |cast.t x3, x3, u_byte
      |st x3, 0(sp)
      |# Half Store
      |cast.t x3, x3, u_half
      |st x3, 2(sp)
      |# Word Store
      |cast.t x3, x3, u_word
      |st x3, 4(sp)
      |
      |# U_Byte Load
      |lbu x5, 0(sp)
      |# S_Byte Load
      |lb x6, 0(sp)
      |# U_Half Load
      |lhu x7, 2(sp)
      |# S_Half Load
      |lh x8, 2(sp)
      |# U_Word Load
      |lwu x9, 4(sp)
      |# S_Word Load
      |lw x10, 4(sp)
      |""".stripMargin

  private val branch_less_than_test =
    """# Init Registers
      |addi x3, x3, -1
      |addi x4, x4, 1
      |
      |# Signed - Signed SLT
      |blt x3, x4, 8
      |jal x1, s_s
      |
      |# Unsigned - Signed SLT
      |cast x5, x3, u_word
      |blt x5, x4, 8
      |jal ra, u_s
      |
      |## Signed - Unsigned SLT
      |blt x4, x5, 8
      |jal ra, s_u
      |
      |## Unsigned - Unsigned SLT
      |cast x5, x3, u_word
      |cast x6, x4, u_word
      |blt x5, x6, 8
      |jal ra, u_u
      |
      |jal x0, end
      |
      |
      |s_s:
      | addi x10, x0, 1
      | jalr x0, x1, 0
      |s_u:
      | addi x11, x0, 1
      | jalr x0, ra, 0
      |u_s:
      | addi x12, x0, 1
      | jalr x0, ra, 0
      |u_u:
      | addi x13, x0, 1
      | jalr x0, ra, 0
      |
      |end:
      |""".stripMargin

  private val branch_greater_eq_test =
    """# Init Registers
      |addi x3, x3, -1
      |addi x4, x4, 1
      |
      |# Signed - Signed SLT
      |bge x3, x4, 8
      |jal x1, s_s
      |
      |# Unsigned - Signed SLT
      |cast x5, x3, u_word
      |bge x5, x4, 8
      |jal ra, u_s
      |
      |## Signed - Unsigned SLT
      |bge x4, x5, 8
      |jal ra, s_u
      |
      |## Unsigned - Unsigned SLT
      |cast x5, x3, u_word
      |cast x6, x4, u_word
      |bge x5, x6, 8
      |jal ra, u_u
      |
      |jal x0, end
      |
      |
      |s_s:
      | addi x10, x0, 1
      | jalr x0, x1, 0
      |s_u:
      | addi x11, x0, 1
      | jalr x0, ra, 0
      |u_s:
      | addi x12, x0, 1
      | jalr x0, ra, 0
      |u_u:
      | addi x13, x0, 1
      | jalr x0, ra, 0
      |
      |end:
      |""".stripMargin

  private val mul_test =
    """# Initialize registers
      |addi x1, x0, 15
      |addi x2, x0, 10
      |addi x3, x0, -8
      |addi x4, x0, 0
      |lui x8, 0b10000
      |lui x9, 0b10000
      |
      |#Mul Low
      |mul x5, x1, x2
      |mul x6, x1, x3
      |mul x7, x3, x3
      |mul x10, x8, x9
      |
      |#Mul High
      |mulh x11, x8, x9
      |mulh x12, x1, x3
      |
      |cast.t x13, x8, u_word
      |cast.t x14, x9, u_word
      |mulh x16, x13, x14
      |
      |addi x17, x0, -1
      |addi x18, x0, 2
      |cast.t x17, x17, u_word
      |cast.t x18, x18, u_word
      |mulh x19, x17, x18
      |
      |addi x20, x0, -2
      |lui x21, 0x80000
      |cast.t x20, x20, s_word
      |cast.t x21, x21, u_word
      |mulh x22, x20, x21
      |""".stripMargin

  private val div_test =
    """
      |addi x1, x0, 15
      |addi x2, x0, 10
      |addi x3, x0, -8
      |addi x4, x0, 0
      |addi x5, x0, 150
      |addi x6, x0, -120
      |lui x7 , 0x80000
      |addi x8, x0, -1
      |addi x9, x0, 2
      |
      |# Signed division
      |# +/+
      |div x10, x5, x2
      |# +/-
      |div x11, x5, x3
      |# -/-
      |div x12, x6, x3
      |
      |# Division by zero
      |div x13, x1, x4
      |
      |# Overflow case
      |div x14, x7, x8
      |
      |# Unsigned division
      |cast.t x6, x6, u_word
      |cast.t x3, x3, u_word
      |div x15, x3, x6
      |
      |# Division by zero
      |cast.t x4, x4, u_word
      |div x16, x5, x4
      |
      |# Large unsigned values
      |cast.t x8, x8, u_word
      |cast.t x9, x9, u_word
      |div x17, x8, x9
      |""".stripMargin

  private val rem_test =
    """
      |addi x1, x0, 15
      |addi x2, x0, 10
      |addi x3, x0, -8
      |addi x4, x0, 0
      |addi x5, x0, 150
      |addi x6, x0, -120
      |lui x7 , 0x80000
      |addi x8, x0, -1
      |addi x9, x0, 2
      |
      |# Signed remainder
      |rem x10, x5, x2
      |rem x11, x5, x3
      |rem x12, x6, x3
      |
      |# Remainder by zero
      |rem x13, x1, x4
      |
      |# Unsigned remainder
      |cast.t x6, x6, u_word
      |cast.t x3, x3, u_word
      |rem x15, x3, x6
      |
      |# Division by zero
      |cast.t x4, x4, u_word
      |rem x16, x5, x4
      |
      |# Large unsigned values
      |cast.t x8, x8, u_word
      |cast.t x9, x9, u_word
      |rem x17, x8, x9
      |""".stripMargin

  private val spill_reload_test =
    """# Init stack pointer
      |lui x2, 0x1
      |
      |# Init Registers
      |addi x3, x0, -1
      |cast.t x3, x3, u_byte
      |addi x4, x0, 10
      |cast.t x4, x4, u_word
      |addi x5, x0, 0xff
      |cast.t x5, x5, s_half
      |addi x6, x0, -500
      |cast.t x6, x6, u_half
      |lui x7, 0xf2
      |
      |
      |# Register Spilling
      |str x3, 0(sp)
      |str x4, 4(sp)
      |str x5, 8(sp)
      |str x6, 12(sp)
      |str x7, 16(sp)
      |# Register Reloading
      |ltr x13, 0(sp)
      |ltr x14, 4(sp)
      |ltr x15, 8(sp)
      |ltr x16, 12(sp)
      |ltr x17, 16(sp)
      |""".stripMargin

  private val cast_test =
    """
      |# Cast Registers Tag
      |addi x3, x0, -1
      |cast.t x4, x3, u_byte
      |addi x5, x0, 10
      |cast.t x6, x5, u_word
      |addi x7, x0, 0xff
      |cast.t x8, x7, s_half
      |addi x9, x0, -500
      |cast.t x10, x9, s_byte
      |lui x11, 0xf2
      |cast.t x12, x11, u_half
      |
      |# Cast Registers and Tag
      |addi x13, x0, -1
      |cast x14, x13, u_byte
      |addi x15, x0, 10
      |cast x16, x15, u_word
      |addi x17, x0, 0xff
      |cast x18, x17, s_half
      |addi x19, x0, -500
      |cast x20, x19, s_byte
      |lui x21, 0xf2
      |cast x22, x21, u_half
      |""".stripMargin

  private val compressed_cast_test =
    """# Cast Registers and Tag
      |c.addi x13, -1
      |c.cast x13, u_byte
      |c.addi x15, 10
      |c.cast x15, u_word
      |c.addi x17, 0xff
      |c.cast x17, s_half
      |c.addi x19, -500
      |c.cast x19, s_byte
      |c.lui x21, 0xf2
      |c.cast x21, u_half
      |""".stripMargin

  private val compressed_spill_reload_test =
    """# Init stack pointer
      |c.lui x30, 0x1
      |c.add x2, x30
      |
      |# Init Registers
      |c.addi x3, -1
      |c.cast x3, u_byte
      |c.addi x4, 10
      |c.cast x4, u_word
      |c.addi x5, 0xff
      |c.cast x5, s_half
      |c.addi x6, -500
      |c.cast x6, u_half
      |c.lui x7, 0xf2
      |
      |
      |# Register Spilling
      |c.strsp x3, 0
      |c.strsp x4, 4
      |c.strsp x5, 8
      |c.strsp x6, 12
      |c.strsp x7, 16
      |# Register Reloading
      |c.ltrsp x13, 0
      |c.ltrsp x14, 4
      |c.ltrsp x15, 8
      |c.ltrsp x16, 12
      |c.ltrsp x17, 16
      |""".stripMargin

  private val context_switch_test =
    """
      |
      |""".stripMargin
  def main(args: Array[String]): Unit = {
    val code = compressed_spill_reload_test
    WorkflowCompiler(code) match {
      case Right(binary) =>
        print(binary(0))
        val mem = new Array[Int](1024 * 256) // 1 MB, also check masking in load and store
        for (i <- binary.indices) {
          mem(i) = binary(i)
        }
        val start = 0x00
        val stop = start + binary.length * 4

        val sim = new SimRV(mem, start, stop)
        sim
    }
  }
}
