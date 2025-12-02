package wildcat.assembler.lexer

import scala.util.parsing.input.Positional

sealed trait TOKEN extends Positional

case class COMMENT(comment: String) extends TOKEN


case class LABEL_DEF(name: String) extends TOKEN
case class LOCAL_LABEL_DEF(n: Int) extends TOKEN
case class LOCAL_LABEL_REF(n: Int, forward: Boolean) extends TOKEN
case class IDENT(name: String) extends TOKEN

case class LITERAL(name: String) extends TOKEN

case class REGISTER(name: String, loc: Int) extends TOKEN
case class IMMEDIATE(raw: String, num: Int) extends TOKEN
case class ITAG(name: String, encoded: Int) extends TOKEN

case class PREDECESSOR(set: String) extends TOKEN
case class SUCCESSOR(set: String) extends TOKEN

case class DIRECTIVES(name: String) extends TOKEN

case class COMMA() extends TOKEN
case class COLON() extends TOKEN
case class LPAREN() extends TOKEN
case class RPAREN() extends TOKEN
case class NEWLINE() extends TOKEN

sealed trait INSTRUCTION_TOKEN extends TOKEN { def name: String}

case class R_TYPE_INSTRUCTION(name: String) extends INSTRUCTION_TOKEN
sealed trait I_TYPE_INSTRUCTION extends INSTRUCTION_TOKEN
case class IMM_ARITH_INSTRUCTION(name: String) extends I_TYPE_INSTRUCTION
case class LOAD_INSTRUCTION(name: String) extends I_TYPE_INSTRUCTION
case class JUMP_LINK_REG_INSTRUCTION(name: String) extends I_TYPE_INSTRUCTION
case class SYSTEM_INSTRUCTION(name: String) extends I_TYPE_INSTRUCTION

case class S_TYPE_INSTRUCTION(name: String) extends INSTRUCTION_TOKEN
case class B_TYPE_INSTRUCTION(name: String) extends INSTRUCTION_TOKEN
case class U_TYPE_INSTRUCTION(name: String) extends INSTRUCTION_TOKEN
case class J_TYPE_INSTRUCTION(name: String) extends INSTRUCTION_TOKEN
case class FENCE(name: String = "fence") extends INSTRUCTION_TOKEN







