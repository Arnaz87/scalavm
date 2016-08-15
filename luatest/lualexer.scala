import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object Lexer {
  abstract class TokenType {
    def unapply (tk: Token): Option[String] = {
      if (tk.t == this) Some(tk.m)
      else None
    }
  }
  case object Kw extends TokenType
  case object Op extends TokenType
  case object Const extends TokenType
  case object Num extends TokenType
  case object Name extends TokenType
  case object Str extends TokenType
  case object Grm extends TokenType

  class Token (
      val t: TokenType,     // type
      val m: String = null, // match
      val p: Int = 0) {     // pos
    def length = m.length
    override def toString =
      s"Token$t($m)"
  }

  case object NullToken extends Token(null, null, 0)

  object Token {
    def unapply (tk: Token) = Some(tk.t, tk.m, tk.p)
  }

  val numrgx = """^([0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?)""".r.unanchored
  val varrgx = """^([a-z][a-zA-Z0-9]*)""".r.unanchored
  val strrgx = """^(\"(?:[^\\^\"]|\\.)*\")""".r.unanchored
  val kws = "do elseif else end for function if in repeat then until while local break return".split(" ")
  val cons = "true false nil ...".split(" ")
  val ops = "+ - * / % ^ == <= >= < > ~= and or not .. #".split(" ")
  val grammar = "; [ ] { } ( ) . , =".split(" ")

  val spcrgx = """^(\s*)""".r.unanchored

  class State (var text: String) {
    var token: Option[Token] = None
    var tokens: ArrayBuffer[Token] = new ArrayBuffer[Token]
    var pos: Int = 0
    def consume (n: Int) {text = text drop n; pos = pos + n}
    def tokenlength = token match {
      case Some(tk) => tk.length
      case None => 0
    }
    def consumespace {
      text match {
        case spcrgx(result) => consume(result.length)
        case _ =>
      }
    }
    def trytoken (st: String, tp: TokenType) {
      if (st.length > tokenlength) token = Some(new Token(tp, st, pos))
    }
    def trymatch (patt: Regex, tp: TokenType) {
      text match { case patt(result) => trytoken(result, tp); case _ => }
    }
    def trymatch (patt: String, tp: TokenType) {
      if (text startsWith patt) { trytoken(patt, tp) }
    }
    def trymatch (patt: Traversable[String], tp: TokenType) {
      patt.foreach{ trymatch(_, tp) }
    }
    def usetoken {
      token match {
        case Some(tk) =>
          consume(tk.m.length)
          tokens += tk
        case None =>
          throw new Error("Lexer Error: Unrecognized token, at " + pos)
      }
      token = None
    }
  }

  def tokenize (str: String): Array[Token] = {
    val state = new State(str)
    while (state.text.length > 0) {
      state.consumespace
      state.trymatch(kws, Kw)
      state.trymatch(ops, Op)
      state.trymatch(cons, Const)
      state.trymatch(numrgx, Num)
      state.trymatch(varrgx, Name)
      state.trymatch(strrgx, Str)
      state.trymatch(grammar, Grm)
      state.usetoken
    }
    return state.tokens.toArray
  }
}