package arnaud.myvm.lua

//import scala.io.Source
//import fastparse.all._
import arnaud.myvm._
import arnaud.myvm.codegen.Codegen

object Ast {
  sealed abstract class Op
  case object Or  extends Op
  case object And extends Op

  case object Add extends Op
  case object Sub extends Op
  case object Mul extends Op
  case object Div extends Op
  case object Mod extends Op

  case object Lt  extends Op
  case object Gt  extends Op
  case object Lte extends Op
  case object Gte extends Op
  case object Eq  extends Op
  case object Neq extends Op

  case object App extends Op
  case object Pow extends Op

  sealed trait Node
  sealed trait expr extends Node
  sealed trait stmt extends Node
  sealed trait literal extends expr
  sealed trait const extends literal
  sealed trait assignable extends Node

  case class Num (v: Double) extends const
  case class Str (v: String) extends const
  case class Bool (v: Boolean) extends const
  case object Nil extends const

  case object Varargs extends literal

  case class TableField (l: Option[expr], r: expr)
  case class Table (fields: Seq[TableField]) extends literal

  case class Function (params: Seq[Var], body: Block) extends literal

  case class Var (name: String) extends expr with assignable
  case class Binop (l: expr, r: expr, op: Op) extends expr
  case class Field (l: expr, name: expr) extends expr with assignable
  case class Call (l: expr, args: Seq[expr]) extends expr with stmt

  case class Block (stmts: Seq[stmt]) extends Node
  case class Assign (l: Seq[assignable], r: Seq[expr]) extends stmt
  case class IfBlock (cond: expr, block: Block)
  case class If (ifs: Seq[IfBlock], orelse: Block) extends stmt
  case class While (cond: expr, body: Block) extends stmt

  def Generate (node: Node): Codegen.Node = {
    import arnaud.myvm.codegen.Codegen.{Nodes => CG}
    node match {
      case Num(v) => CG.Constant(v)
      case Str(v) => CG.Constant(v)
      case Bool(v) => CG.Constant(v)

      case Var(name) => CG.GlobalDynVar(name)
      case Binop(l, r, op) =>
        val $l = Generate(l)
        val $r = Generate(r)
        op match {
          //case Or  => CG.Bin(Machine.Or , $l, $r)
          //case And => CG.Bin(Machine.And, $l, $r)

          case Add => CG.Bin(Instruction.Add, $l, $r)
          case Sub => CG.Bin(Instruction.Sub, $l, $r)
          case Mul => CG.Bin(Instruction.Mul, $l, $r)
          case Div => CG.Bin(Instruction.Div, $l, $r)

          case Eq  => CG.Bin(Instruction.Eq , $l, $r)
          case Neq => CG.Bin(Instruction.Neq, $l, $r)
          case Lt  => CG.Bin(Instruction.Lt , $l, $r)
          case Lte => CG.Bin(Instruction.Lte, $l, $r)
          case Gt  => CG.Bin(Instruction.Lt , $r, $l)
          case Gte => CG.Bin(Instruction.Lte, $r, $l)

          case App => CG.Call(CG.Var("_append"), CG.Narr(Array($l, $r)))
        }
      case Call(f, args) => CG.Call(Generate(f), CG.Narr(args.map{Generate _}))
      case Assign(l, r) => CG.Assign(Generate(l.head), Generate(r.head))

      case Block(xs) => CG.Block(xs.map{Generate _})
      case While(cons, body) => CG.While(Generate(cons), Generate(body))
      case If(conds, orelse) => conds.head match {
        case IfBlock(cond, block) =>
          CG.If(Generate(cond), Generate(block), Generate(orelse))
      }
    }
  }
}

object Lexical {
  import fastparse.all._
  val number = {
    sealed abstract class Sign
    case object Positive extends Sign
    case object Negative extends Sign
    def divideTens (n: Double, i: Int): Double =
      if (i < 1) {n} else { divideTens(n/10, i-1) }
    def multiplyTens (n: Double, i: Int): Double =
      if (i < 1) {n} else { multiplyTens(n*10, i-1) }
    val digits = P( CharIn('0' to '9').rep(1).! )
    val sign =
      P(("+"|"-").!.?.map(_ match {
        case Some("+") => Positive
        case Some("-") => Negative
        case None => Positive
        case _ => ???
      }))
    val intpart =
      P(sign ~ digits).map(_ match {
        case (Positive, digits) => digits.toInt
        case (Negative, digits) => -(digits.toInt)
      })
    val realpart =
      P(intpart ~ ("." ~ digits).?).map(_ match {
        case (intpart, None) => intpart.toDouble
        case (intpart, Some(fracpart)) =>
          intpart.toDouble + divideTens(fracpart.toDouble, fracpart.length)
      })
    P( realpart ~ (("e"|"E") ~ sign ~ digits ).?).map(
      _ match {
        case (realpart, None) => realpart
        case (realpart, Some((Positive, expdigits))) =>
          multiplyTens(realpart, expdigits.toInt)
        case (realpart, Some((Negative, expdigits))) =>
          divideTens(realpart, expdigits.toInt)
      }
    ).map(Ast.Num)
  }

  val string = {
    // Esta definición de strings me gusta, pero no es la que usa Lua.
    val validChars = P(!("\\" | "\"") ~ AnyChar.!)
    val uescape = P("\\u" ~/ AnyChar.rep(min=4,max=4).!).map( _.toInt.toChar)
    val xescape = P("\\x" ~/ AnyChar.rep(min=2,max=2).!).map( _.toInt.toChar)
    val escape = P("\\" ~ !("u"|"x"|"z") ~ AnyChar.!).map(_ match {
      case "n" => "\n"
      case "t" => "\t"
      case c => c
    })
    P( "\"" ~/ (validChars|uescape|xescape|escape).rep.map(_.mkString) ~/ "\"" ).map(Ast.Str)
  }

  val const = P(
    kw("true").map( _ => Ast.Bool(true)) |
    kw("false").map(_ => Ast.Bool(false)) |
    kw("nil").map( _ => Ast.Nil)
  )

  val varargs = P("...").map(_ => Ast.Varargs)

  /* Lista de palabras claves:
     operadores: and or not
     constantes: true false nil
     reservadas: do elseif else end for function if in repeat then until while local break return
     añadidas en lua 5.3: goto
     Si se cambia esta lista, cambiar el string de abajo también
  */
  val keywords: Set[String] = "and or not true false nil do elseif else end for function if in repeat then until while local break return goto".split(' ').toSet

  val namechar = CharIn('a' to 'z', 'A' to 'Z', '0' to '9')
  val name = P(CharIn('a' to 'z') ~ namechar.rep).!.filter(!keywords.contains(_))

  def kw (str: String) = P(str ~ !(namechar))

  val comment = P("--" ~ CharsWhile(_ != '\n'))
  val ws = P(CharsWhile(" \n\t".toSet))
  val wscomment = P( (ws|comment).rep )

  //val ops = P("+"|"-"|"*"|"/"|"%"|"^"|"=="|"<="|">="|"<"|">"|"~="|"and"|"or"|"not"|".."|"#")
  /* Tokens especiales:
    +     -     *     /     %     ^     #
    ==    ~=    <=    >=    <     >     =
    (     )     {     }     [     ]
    ;     :     ,     .     ..    ...
    añadidos en lua 5.3:
    &     ~     |     <<    >>    //    ::
  */
  /* Precedencias de operadores:
    1: or
    2: and
    3: < > <= >= ~= == (Lua usa ~= en vez de !=)
    4: .. (asocia a la derecha)
    5: + -
    6: * / %
    7: not # - (todos los unarios)
    8: ^ (asocia a la derecha)

    Añadidos en Lua 5.3:
    3.1: |
    3.2: ~
    3.3: &
    3.4: << >>
    6: //
    7: ~ (unario)
  */
}

import fastparse.noApi._
object WsApi extends fastparse.WhitespaceApi.Wrapper(Lexical.wscomment)
import WsApi._
//import fastparse.all._

import Lexical.{kw => Kw}

object Ops {
  // "prec" es de "precedence" (precedencia)
  case class Prec (op: Ast.Op, prec: Int)
  def op(p: P[_], o: Ast.Op, prec: Int): P[Prec] = {
    val oprec = Prec(o, prec)
    p.map{ _ => oprec }
  }
  def op(s: String, o: Ast.Op, prec: Int): P[Prec] = { op(P(s), o, prec) }

  val NilOp = Prec(null, 0)

  // Ningún operador debe ser 0
  val Or  = op(Kw("or"), Ast.Or, 1)
  val And = op(Kw("and"), Ast.And, 2)
  val Lt = op("<", Ast.Lt, 3)
  val Gt = op(">", Ast.Gt, 3)
  val Lte = op("<=", Ast.Lte, 3)
  val Gte = op(">=", Ast.Gte, 3)
  val Eq = op("==", Ast.Eq, 3)
  val Neq = op("~=", Ast.Neq, 3)

  val App = op("..", Ast.App, 4)
  val Add = op("+", Ast.Add, 5)
  val Sub = op("-", Ast.Sub, 5)
  val Mul = op("*", Ast.Mul, 6)
  val Div = op("/", Ast.Div, 6)

  val ops = P( Or | And | Lt | Gt | Lte | Gte | Eq | Neq | App | Add | Sub | Mul | Div )

  def helper_with_state (firstValue: Ast.expr, pairs: Seq[(Prec, Ast.expr)]): Ast.expr = {
    import scala.collection.mutable.Stack
    val values = new Stack[Ast.expr]
    val ops = new Stack[Prec]
    values.push(firstValue)
    ops.push(NilOp)

    def push_op (op: Prec) {
      // Si el último operador no es importante no hay que calcular nada.
      // Si hay menos de dos valores, no se puede calcular nada.
      // La pila de operadores nunca estará vacía, siempre habrá un NilOp
      // porque tiene precedencia 0, así que no hay que preocuparse por eso.
      if (op.prec > ops.head.prec || values.length < 2) {
        ops.push(op)
      } else {
        val top = ops.pop().op
        val b = values.pop()
        val a = values.pop()
        values.push(Ast.Binop(a, b, top))
        // Repetir el proceso hasta que el último operador no sea importante.
        push_op(op)
      }
    }

    pairs.foreach{
      case(op, value) =>
        push_op(op)
        values.push(value)
    }
    push_op(NilOp)
    values.pop()
  }

  def expr (atom: P[Ast.expr]): P[Ast.expr] = {
    P(atom ~ (ops ~ atom).rep).map{
      case (a, bs) => helper_with_state(a, bs)
    }
  }
}

object Expressions {
  val variable = P(Lexical.name).map(Ast.Var)
  val const = P(Lexical.number | Lexical.const | Lexical.string)

  private object Prefix {
    sealed abstract class Suffix {
      def toExpr (l: Ast.expr): Ast.expr
    }
    case class Call (rs: Seq[Ast.expr]) extends Suffix {
      def toExpr (l: Ast.expr) = Ast.Call(l, rs)
    }
    case class Field (r: Ast.expr) extends Suffix {
      def toExpr (l: Ast.expr) = Ast.Field(l, r)
    }

    val $call = P("(" ~/ expr.rep(sep = ",").map(Call)  ~ ")")
    val $field = P("[" ~/ expr.map(Field) ~ "]")
    // El !".." es necesario aquí para no confundirlo con el operador de concatenación
    val $dot = P((!".." ~ ".") ~/ Lexical.name.map(Ast.Str).map(Field))
    val prefix =
      P(variable ~ ($call | $dot | $field).rep).map{ case (v, ss) =>
        ss.foldLeft[Ast.expr](v) { (nv, s) => s.toExpr(nv) }
      }
  }

  val prefix = Prefix.prefix
  val call =
    P(prefix.filter(_.isInstanceOf[Ast.Call])
            .map(_.asInstanceOf[Ast.Call]))
  val assignable =
    P(prefix.filter(_.isInstanceOf[Ast.assignable])
            .map(_.asInstanceOf[Ast.assignable]) )

  private val fieldsep = P(","|";")
  private val tablefield =
    P((("[" ~/ expr ~ "]" | Lexical.name.map(Ast.Str)) ~ "=").? ~ expr)
    .map(Ast.TableField.tupled)
  var table = P("{" ~/ tablefield.rep(sep=fieldsep) ~ "}").map(Ast.Table)

  var funcbody =
    P("(" ~ variable.rep(sep=",") ~ ")" ~
      Statements.block ~ Kw("end")).map(Ast.Function.tupled)
  var function = P(Kw("function") ~/ funcbody)

  val inparen = P("(" ~/ expr ~ ")")
  val atom = P( prefix | variable | const | inparen | table | function )
  val expr: P[Ast.expr] = Ops.expr(atom);
}

object Statements {
  import Expressions.expr

  val assign: P[Ast.Assign] =
    P( Expressions.assignable.rep(min=1, sep=",") ~ "=" ~/
       Expressions.expr.rep(min=1, sep=",")).map(Ast.Assign.tupled)
  val call: P[Ast.Call] = Expressions.call
  val doblock: P[Ast.Block] = P(Kw("do") ~/ block ~ Kw("end"))

  val ifstmt: P[Ast.If] =
    P(Kw("if") ~/ expr ~
      Kw("then") ~/ block ~
      (Kw("elseif") ~/ expr ~ Kw("then") ~ block).rep ~
      (Kw("else") ~/ block).? ~
      Kw("end")).map{
        case (cnd, blck, elss, els) =>
          val ifs = elss.map{ case (c,b) => Ast.IfBlock(c,b) } :+ Ast.IfBlock(cnd, blck)
          val orelse = els match {
            case Some(bl: Ast.Block) => bl
            case None => Ast.Block(Nil)
          }
          Ast.If(ifs, orelse)
      }

  val whilestmt: P[Ast.While] =
    P(Kw("while") ~/ expr ~ Kw("do") ~ block ~ Kw("end")).map(Ast.While.tupled)

  val function = P(Kw("function") ~/ Lexical.name ~ Expressions.funcbody).map{
    case (name, fun) => Ast.Assign(Seq(Ast.Var(name)), Seq(fun))
  }

  val block: P[Ast.Block] = P(stmt.rep(sep=P(";".?), min=1)).map(Ast.Block)

  val stmt: P[Ast.stmt] = P(assign | call | ifstmt | whilestmt | function)
}

object Main {
  
  def parse_text (text: String): Ast.Node = {
    //Expressions.expr.parse(text)
    P(Statements.block ~ End).parse(text) match {
      case Parsed.Success(succ, _) => succ
      case fail: Parsed.Failure =>
        print(fail.extra.traced.trace)
        System.exit(0)
        ???
    }
  }

  def parse_file (name: String) = {
    parse_text(scala.io.Source.fromFile(name).mkString)
  }

  def manual (): Nothing = {
    println("Usage: (-i <code> | -f <filename>) [-debug <level>] [-print-parsed] [-print-nodes] [-print-code]")
    System.exit(0)
    return ???
  }

  class Params (input: List[String]) {
    val data = collection.mutable.Map[String, Option[String]]()
    private def helper (l: List[String]) {
      l match {
        case k::v::xs if
          (k startsWith "-") && !(v startsWith "-") =>
          data(k.tail) = Some(v); helper(xs)
        case k::xs if k startsWith "-" =>
          data(k.tail) = None; helper(xs)
        case Nil =>
        case _::xs => helper(xs)
      }
    }
    helper(input)

    def apply (k: String) = data(k).get
    def get (k: String, other: String) = data.getOrElse(k, None).getOrElse(other)
    def has (k: String) = data.contains(k)
  }

  def main (args: Array[String]) {
    val params = new Params(args.toList)
    /*val parsed = args(0) match {
      case "-f" => parse_file(args(1))
      case "-i" => parse_text(args(1))
      case _ => manual()
    }
    var nextparam = 2
    val debug = if (args.length >= 4 && args(2) == "-debug")
      {args(3).toInt} else {0}*/
    val parsed =
      if (params.has("f"))
      { parse_file(params("f")) }
      else if (params.has("i"))
      { parse_text(params("i")) }
      else { manual() }
    if (params.has("print-parsed")) { println(parsed) }
    val debug = params.get("debug", "0").toInt
    val _codenode = Ast.Generate(parsed)
    val codenode = _codenode match {
      case Codegen.Nodes.Block(xs) =>
        Codegen.Nodes.Block(
          Codegen.Nodes.Assign(
            Codegen.Nodes.DynVar("_append"),
            Codegen.Nodes.Mod("Lua", "append")
          ) +:
          Codegen.Nodes.Assign(
            Codegen.Nodes.DynVar("print"),
            Codegen.Nodes.Mod("Lua", "print")
          ) +: xs
        )
    }
    if (params.has("print-nodes")) { println(codenode) }
    val codestate = new Codegen.State()
    val luamodule = Map(
      "print" -> Runnable({ (_args) =>
        //Machine.states.top.regs.printValues()
        val args = _args.asInstanceOf[Dict]
        println(args("0"))
      }),
      "append" -> Runnable({ (_args) =>
        val args = _args.asInstanceOf[Dict]
        val a = args("0").toString
        val b = args("1").toString
        a + b
      })
    )
    Machine.modules("Lua") = Codegen.makeModule(luamodule, "Lua")
    Machine.modules("Main") = codestate.generate(codenode)
    if (params.has("print-code")) {
      Machine.modules("Main").data.dyn("main").asInstanceOf[Code].printValues
    }
    Machine.debug(debug)
    try {
      Machine.start()
    } catch {
      case e: Throwable => {
        println("ERROR!")
        println("REGISTER VALUES")
        Machine.states.top.regs.printValues()
        println("STACK TRACE")
        e.printStackTrace()
      }
    }
  }
}