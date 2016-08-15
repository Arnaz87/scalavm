import scala.io.Source

abstract class Assoc
case object Left extends Assoc
case object Right extends Assoc
case object NonAssoc extends Assoc

sealed class Op (val str: String, val prec: Int, val assoc: Assoc = Left) {
  def unapply (other: Op) = other eq this
  override def toString () = s"Op($str)"
}

object Op {
  case object NullOp extends Op(null, -1, NonAssoc)

  val ops = Array(
    new Op("or", 1),
    new Op("and", 2),
    new Op("<", 3),
    new Op(">", 3),
    new Op("<=", 3),
    new Op(">=", 3),
    new Op("~=", 3), // prefiero !=, pero lua usa esto...
    new Op("==", 3),
    new Op("..", 4),
    new Op("+", 5),
    new Op("-", 5),
    new Op("*", 6),
    new Op("/", 6),
    new Op("**",7, Right)
  )
  // apply falla si ningún operador usa str
  def apply (str: String) = ops.find(_.str == str).get
}

object Ast {
  sealed trait Node
  sealed trait expr extends Node
  sealed trait stmt extends Node
  sealed trait literal extends expr
  sealed trait const extends literal
  sealed trait assignable

  case class Num (v: Double) extends const
  case class Str (v: String) extends const
  case class Bool (v: Boolean) extends const
  case object NilLit extends const

  case object Varargs extends literal

  case class Var (name: String) extends expr with assignable
  case class Field (l: expr, name: expr) extends expr with assignable
  case class Call (l: expr, args: Seq[expr]) extends expr with stmt
  case class Binop (l: expr, r: expr, op: Op) extends expr

  case class Block (stmts: Seq[stmt]) extends Node
  case class Assign (l: assignable, r: expr) extends stmt
  case class If (cond: expr, body: Block, orelse: Block) extends stmt
  case class While (cond: expr, body: Block) extends stmt
}

class Parser (inp: Seq[Lexer.Token]) {
  var tokens = inp

  def tkhead = tokens.headOption.getOrElse(Lexer.NullToken)

  def consume () { tokens = tokens.tail }
  def eof = tokens.length == 0

  def fail (msg: String) =
    throw new Error("Error: " + msg + ", at:" + tkhead.p)
  def WTF (msg: String) =
    throw new Error("WTF! " + msg + ", at:" + tkhead.p)
  def WTF (): Nothing = WTF("")

  def parse_any [T <: Ast.Node](funs: (()=>Option[T])*): Option[T] = {
    if (eof) return None
    for (f <- funs) {
      f() match {
        case Some(r) => return Some(r)
        case _ =>
      }
    }
    return None
  }

  def parse_between [T](fun: ()=>T, a: String, b: String): Option[T] = {
    // En un match, la sintaxis `a` hace que se comparen los valores, en vez
    // de usarlo como variable para guardar los valores.
    // http://stackoverflow.com/questions/25344564/pattern-matching-on-non-literal-values
    tkhead match {
      case Lexer.Grm(`a`) =>
        consume()
        val r = fun()
        tkhead match {
          case Lexer.Grm(`b`) => consume(); Some(r)
          case _ => fail("Expecting closing string \""+b+"\"")
        }
      case _ => None
    }
  }

  def expect [T](Tk: Lexer.TokenType, q: String) {
    tkhead match {
      case Tk(`q`) => consume()
      case _ => fail(s"Expected token $Tk matching $q")
    }
  }

  def parse_const (): Option[Ast.const] = {
    val token = tokens.head
    def consome[T] (node: T) = { consume; Some(node) }
    tkhead match {
      case Lexer.Num(n) => consome(Ast.Num(n.toDouble))
      case Lexer.Const("true") => consome(Ast.Bool(true))
      case Lexer.Const("false") => consome(Ast.Bool(false))
      case Lexer.Const("nil") => consome(Ast.NilLit)
      case Lexer.Str(str) => consome(Ast.Str(str))
      case _ => None
    }
  }

  def parse_var (): Option[Ast.Var] = {
    tkhead match {
      case Lexer.Name(str) =>
        consume()
        Some(Ast.Var(str))
      case _ => None
    }
  }

  def parse_var_call (): Option[Ast.expr] = {
    def helper (last: Ast.expr): Ast.expr = {
      tkhead match {
        case Lexer.Grm(".") =>
          consume()
          tkhead match {
            case Lexer.Name(m) =>
              consume()
              helper(Ast.Field(last, Ast.Str(m)))
            case _ => fail("Expecting field name")
          }
        case Lexer.Grm("[") =>
          parse_between(parse_expr _, "[", "]") match {
            case Some(Some(expr)) => helper(Ast.Field(last, expr))
            case Some(None) => fail("Expecting expression")
            case None => WTF
          }
        case Lexer.Grm("(") =>
          parse_between(parse_expr_list _, "(", ")") match {
            case Some(exprlist) => helper(Ast.Call(last, exprlist))
            case None => WTF
          }
        case _ => last
      }
    }
    parse_var().map{helper(_)}
  }

  def parse_call_assign (): Option[Ast.stmt] = {
    parse_var_call() match {
      case Some(call: Ast.Call) => Some(call)
      case Some(left: Ast.assignable) =>
        // TODO: Asignación múltiple
        expect(Lexer.Grm, "=")
        parse_expr match {
          case Some(right) => Some(Ast.Assign(left, right))
          case None => fail("Expecting expression to assign")
        }
      case None => None
      case _ => WTF("Analizando una asignación salió algo que no es una función ni un asignable")
    }
  }

  def parse_expr_list (): List[Ast.expr] = {
    parse_expr match {
      case Some(expr) =>
        tkhead match {
          case Lexer.Grm(",") =>
            consume()
            expr :: parse_expr_list()
          case _ => List(expr)
        }
      case None => List()
    }
  }
  
  def parse_if (): Option[Ast.If] = {
    def parse_if_stuff (): Ast.If = {
      parse_expr() match {
        case Some(cond) => 
          expect(Lexer.Kw, "then")
          val body = parse_block()
          tkhead match {
            case Lexer.Kw("end") =>
              consume()
              Ast.If(cond, body, Ast.Block(List()))
            case Lexer.Kw("else") =>
              consume()
              val orelse = parse_block()
              expect(Lexer.Kw, "end")
              Ast.If(cond, body, orelse)
            case Lexer.Kw("elseif") =>
              consume()
              var orelse = parse_if_stuff()
              Ast.If(cond, body, Ast.Block(List(orelse)))
            case _ => fail("Expecting end, else or elseif keywords")
          }
        case None => fail("Expecting condition expression")
      }
    }
    tkhead match {
      case Lexer.Kw("if") =>
        consume()
        Some(parse_if_stuff)
      case _ => None
    }
  }

  def parse_while (): Option[Ast.While] = {
    tkhead match {
      case Lexer.Kw("while") =>
        consume()
        parse_expr() match {
          case Some(cond) =>
            expect(Lexer.Kw, "do")
            val body = parse_block()
            expect(Lexer.Kw, "end")
            Some(Ast.While(cond, body))
          case None => fail("Expecting condition expresion")
        }
      case _ => None
    }
  }

  def parse_block (): Ast.Block = {
    def helper (): Seq[Ast.stmt] = {
      parse_stmt() match {
        case Some(stmt) =>
          tkhead match {
            case Lexer.Grm(";") => consume()
            case _ =>
          }
          stmt +: helper()
        case None => List()
      }
    }
    Ast.Block(helper())
  }

  def parse_atom (): Option[Ast.expr] = {
    parse_any(
      parse_var_call _,
      parse_const _,
      (() => parse_between((() => parse_expr().get), "(", ")"))
    )
  }

  def parse_stmt (): Option[Ast.stmt] = {
    parse_any(parse_call_assign _, parse_if _, parse_while _)
  }

  def parse_expr (): Option[Ast.expr] = {
    def get_op () = {
      tkhead match {
        case Lexer.Op(opstr) =>
          consume()
          Some(Op(opstr))
        case _ => None
      }
    }

    sealed abstract class ConsumeTurn
    case object ValTurn extends ConsumeTurn
    case object OpTurn extends ConsumeTurn

    def push_op(values: List[Ast.expr], ops: List[Op],
                op: Op): (List[Ast.expr], List[Op]) = {
      if (ops.isEmpty || op.prec > ops.head.prec) (values, op::ops)
      else (values, ops) match {
        case (b::a::vs, nop::os) =>
          push_op(Ast.Binop(a,b,nop)::vs, os, op)
        case _ => (values, op::ops)
      }
    }

    // Todo este juego con ConsumeTurn es porque esta función en realidad son
    // dos funciones, una para agregar un valor y otra para agregar un
    // operador, ambas mutualmente recursivas, pero como scala no soporta
    // recursión mutua tengo que simularla de este modo.
    def helper (values: List[Ast.expr],
                ops: List[Op],
                turn: ConsumeTurn): Ast.expr = {
      turn match {
        case ValTurn =>
          parse_atom() match {
            case Some(expr) => helper(expr :: values, ops, OpTurn)
            case None => fail("Expecting expression")
          }
        case OpTurn =>
          get_op() match {
            case Some(op) =>
              push_op(values, ops, op) match {
                case (vs, os) => helper(vs, os, ValTurn)
              }
            case None =>
              push_op(values, ops, Op.NullOp) match {
                case (List(v), List(Op.NullOp)) => v
                case _ => fail("Mismatch of operators and operands")
              }
          }
      }
    }
    parse_atom() match {
      case Some(expr) =>
        get_op() match {
          case Some(op) =>
            Some(helper(List(expr), List(op), ValTurn))
          case None => Some(expr)
        }
      case _ => None
    }
  }
}

object Parser {
  def myprint (obj: Any): String = obj match {
    case Ast.Binop(l, r, op) =>
      "" + myprint(l) + " " + myprint(r) + " " + myprint(op) + ""
    case Ast.Num(n) => n.toString
    case Ast.Var(name) => name
    case op: Op => op.str
    case Some(x) => myprint(x)
    case _ => obj.toString
  }

  def parse_text (text: String) = {
    (new Parser(Lexer.tokenize(text))).parse_block
  }

  def parse_file (name: String) = {
    parse_text(scala.io.Source.fromFile(name).mkString)
  }

  def postfix (text: String) = myprint(parse_text(text))

  def manual (): Nothing = {
    println("Usage: -i <code> | -f <filename>")
    System.exit(0)
    return ???
  }
  def main_no (args: Array[String]) {
    if (args.length != 2) manual()
    val result: Ast.Block = args(0) match {
      case "-f" => parse_file(args(1))
      case "-i" => parse_text(args(1))
      case _ => manual()
    }
    println(result.stmts.mkString("\n\n"))
    // 1 or 2+3 and 4*5+6+7*8
    // debería resultar en:
    // 1 2 3 + 4 5 * 6 + 7 8 * + and or
    // (1 ((2 3 +) (((4 5 *) 6 +) (7 8 *) +) and) or)
    // o en infix:
    // (1 or ((2+3) and (((4*5)+6)+(7*8))))
  }
}
