package wildcat.assembler.lexer

import wildcat.assembler.compiler.{Location, WorkflowLexerError}

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object WorkflowLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r
  private val hex: Regex = "-?0x([0-9a-fA-F]+)".r
  private val bin: Regex = "-?0b([01]+)".r
  private val int: Regex = "(-?[0-9]+)".r


  def apply(code: String): Either[WorkflowLexerError, List[TOKEN]] = {
    parse(token, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def token:  Parser[List[TOKEN]] =
    phrase(rep1(
      comment |                            // consume to end-of-line
      directive |
      labelDef | localLabelDef |                         // e.g., 1:
      comma | leftparenthesis | rightparenthesis | newline |
      literal
      //register |                           // x0..x31, a0..a7, etc.
      //iType | rType | sType | bType | jType | uType | fenceType | // if you keep these
      //immediate |                          // -? (0x.. | 0b.. | [0-9]+)
      //ident |                              // generic identifier / symbol

    ))


//  def register: Parser[REGISTER] = positioned {
//    "x(3[01]|[12][0-9]|[0-9])|zero|ra|sp|gp|tp|t[0-6]|s([0-9]|1[01])|a[0-7]|fp".r ^^ { name =>
//      val loc = name match {
//        case s"x$n"       => n.toInt
//        case "zero"       => 0
//        case "ra"         => 1
//        case "sp"         => 2
//        case "gp"         => 3
//        case "tp"         => 4
//        case "t0"         => 5
//        case "t1"         => 6
//        case "t2"         => 7
//        case "s0" | "fp"  => 8
//        case "s1"         => 9
//        case "a0"         => 10
//        case "a1"         => 11
//        case "a2"         => 12
//        case "a3"         => 13
//        case "a4"         => 14
//        case "a5"         => 15
//        case "a6"         => 16
//        case "a7"         => 17
//        case "s2"         => 18
//        case "s3"         => 19
//        case "s4"         => 20
//        case "s5"         => 21
//        case "s6"         => 22
//        case "s7"         => 23
//        case "s8"         => 24
//        case "s9"         => 25
//        case "s10"        => 26
//        case "s11"        => 27
//        case "t3"         => 28
//        case "t4"         => 29
//        case "t5"         => 30
//        case "t6"         => 31
//      }
//      REGISTER(name, loc)
//    }
//  }
//
//  def immediate: Parser[IMMEDIATE] = positioned {
//    "-?(0x[0-9a-fA-F]+|0b[01]+|[0-9]+)".r ^^ { str =>
//      val num = str match {
//        case hex(n) => Integer.parseInt(n, 16) * (if (str.startsWith("-")) -1 else 1)
//        case bin(n) => Integer.parseInt(n, 2)  * (if (str.startsWith("-")) -1 else 1)
//        case int(n) => Integer.parseInt(n, 10)
//      }
//      IMMEDIATE(str, num)
//    }
//  }
//
//
//  def predecessor: Parser[PREDECESSOR] = positioned {
//    "[iorw]+".r ^? (
//      {
//        case str if str.distinct == str => PREDECESSOR(str)
//      },
//      str => s"Invalid predecessor set '$str': contains duplicate characters"
//    )
//  }
//
//  def successor: Parser[SUCCESSOR] = positioned {
//    "[iorw]+".r ^? (
//      {
//        case str if str.distinct == str => SUCCESSOR(str)
//      },
//      str => s"Invalid successor set '$str': contains duplicate characters"
//    )
//  }

  // --- identifiers & labels ---

  def comment: Parser[COMMENT] = {
    """#.*""".r ^^ { c => COMMENT(c) }
  }

  def literal: Parser[LITERAL] = positioned {
    // Accept .L... style too; disallow starting with digit
    """[A-Za-z0-9-_.$?]+""".r ^^ { s => LITERAL(s) }
  }

  // Named or .L* label definition: IDENT followed immediately by colon
  def labelDef: Parser[LABEL_DEF] = positioned {
    // Use ~< to require a colon after, without consuming it if you want to keep COLON as token,
    // or just consume ":" here and don't emit a separate COLON token.
    ("""[A-Za-z_.$?][A-Za-z0-9_.$?]*""".r <~ ":") ^^ { s => LABEL_DEF(s) }
  }

  // Numeric label definition: e.g., "1:"
  def localLabelDef: Parser[LOCAL_LABEL_DEF] = positioned {
    ("""[0-9]+""".r <~ ":") ^^ { s => LOCAL_LABEL_DEF(s.toInt) }
  }

  // Numeric local label reference: e.g., "1f" or "2b"
  def localLabelRef: Parser[LOCAL_LABEL_REF] = positioned {
    ("""([0-9]+)([fb])""".r) ^^ {
      case s =>
        val n = s.init.toInt
        val forward = if (s.last == 'f') true else false
        LOCAL_LABEL_REF(n, forward)
    }
  }

  // Characters
  def comma           = positioned { ","             ^^ (_ => COMMA()) }
  def colon           = positioned { ":"             ^^ (_ => COLON()) }
  def newline         = positioned { "\n"           ^^ (_ => NEWLINE()) }
  def leftparenthesis = positioned { "("           ^^ (_ => LPAREN()) }
  def rightparenthesis = positioned { ")"           ^^ (_ => RPAREN()) }


  // Directives
  def directive: Parser[DIRECTIVES] = positioned {
    (".text" | ".data" | ".word" | ".half" | ".byte" | ".align" | ".global" | ".string") ^^ {
      dir => DIRECTIVES(dir)
    }
  }

  // R-Type
  def rType: Parser[R_TYPE_INSTRUCTION] = positioned {
    ("add" | "sub" | "xor" | "or" | "and" | "sll" | "srl" | "sra" | "sltu" | "slt" |
        "mulhsu" | "mulhu" | "mulh" | "mul" | "divu" | "div" | "remu" | "rem" ) ^^ {
      instr => R_TYPE_INSTRUCTION(instr)
    }
  }

  // I-Type
  def iType: Parser[I_TYPE_INSTRUCTION] = positioned {
    immArithmInstruction | loadInstruction | jumpLinkRegInstruction | systemInstruction
  }

  def immArithmInstruction: Parser[IMM_ARITH_INSTRUCTION] = positioned {
    ("addi" | "xori" | "ori" | "andi" | "slli" | "srli" | "srai" | "sltiu" | "slti" ) ^^ {
      instr => IMM_ARITH_INSTRUCTION(instr)
    }
  }
  def loadInstruction: Parser[LOAD_INSTRUCTION] = positioned {
    ( "lbu"  | "lhu"  | "lb"   | "lh"   | "lw" ) ^^ {
      instr => LOAD_INSTRUCTION(instr)
    }
  }
  def jumpLinkRegInstruction: Parser[JUMP_LINK_REG_INSTRUCTION] = positioned {
    "jalr" ^^ {
      instr => JUMP_LINK_REG_INSTRUCTION(instr)
    }
  }
  def systemInstruction: Parser[SYSTEM_INSTRUCTION] = positioned {
    ( "ecall" | "ebreak"  ) ^^ {
      instr => SYSTEM_INSTRUCTION(instr)
    }
  }

  // S-Type (stores)
  def sType: Parser[S_TYPE_INSTRUCTION] = positioned {
    ("sb" | "sh" | "sw") ^^ {
      instr => S_TYPE_INSTRUCTION(instr)
    }
  }

  // B-Type (branches)
  def bType: Parser[B_TYPE_INSTRUCTION] = positioned {
    ("bltu" | "bgeu" | "beq" | "bne" | "blt" | "bge") ^^ {
      instr => B_TYPE_INSTRUCTION(instr)
  }
  }

  // J-Type
  def jType: Parser[J_TYPE_INSTRUCTION] = positioned {
    ("jal") ^^ {
      instr => J_TYPE_INSTRUCTION(instr)
    }
  }

  // U-Type
  def uType: Parser[U_TYPE_INSTRUCTION] = positioned {
    ("lui" | "auipc") ^^ {
      instr => U_TYPE_INSTRUCTION(instr)
    }
  }

  // FENCE
  def fenceType: Parser[FENCE] = positioned {
    ("fence") ^^ {
      _ => FENCE()
    }
  }




}
