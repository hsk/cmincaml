package mincaml;
import compact.Compact;

object Parse {
  import mincaml.Syntax._;
  def apply(x:String):T = {
    val tree:Any = Compact.parse(x)
    g(tree)
  }
  def g(x:Any):T = x match {
    case () => Unit()
    case a:Boolean => Bool(a)
    case a:scala.Int => Int(a)
    case a:scala.Float => Float(a)
    case ("!", a) => Not(g(a))
    case ("-", a) => Neg(g(a))
    case (a, "+", b) => Add(g(a),g(b))
    case (a, "-", b) => Sub(g(a),g(b))
    case (".-", a) => FNeg(g(a))
    case (a, ".+", b) => FAdd(g(a),g(b))
    case (a, ".-", b) => FSub(g(a),g(b))
    case (a, ".*", b) => FMul(g(a),g(b))
    case (a, "./", b) => FDiv(g(a),g(b))
    case (a, "==", b) => Eq(g(a), g(b))
    case (a, "!=", b) => LE(g(a), g(b))
    case ("if", "(", a, ")", (b, "else", c)) => If(g(a), g(b), g(c))
    case ("if", "(", a, ")", b) => If(g(a), g(b), Unit())
    case ("val", (a:String, "=", b)) => Let((a, Type.gentyp()), g(a), g(b))
    case b:String => Var(b)
    case ((name:String, ":", ("def", "(",p,")", body)), ";", next) =>
      LetRec(
        Fundef(
          (name,Type.gentyp()),
          paramlist(p),
          g(body)
        ), g(next)
      )
    case ("(", r@(a, ",", b) , ")") => Tuple(tuplelist(r))
    case (a, "=", ("let","(",b,")", c)) =>
      val bb = paramlist(b);
      LetTuple(bb ,g(a), g(c))
    case ("array", "(", a, ",", b, ")") => Array(g(a), g(b))
    case ((a, "[", b, "]"),"=", c) => Put(g(a), g(b), g(c))
    case (a, "=", b) => App(g(a), exps(b))
    case (a, "[", b, "]") => Get(g(a), g(b))
  }
  def exps(x:Any):List[T] = x match {
    case (a, "in", b) => exps(a):::exps(b)
    case a => List(g(a))
  }
  def paramlist(x:Any):List[(Id.T,Type.T)] = x match {
    case a:String  => List((a,Type.Var(None)))
    case (a,",",b) => paramlist(a) ::: paramlist(b)
    case _         => throw new Exception("error")
  }
  def tuplelist(x:Any):List[T] = x match {
    case (a, ",", b) => tuplelist(a) ::: tuplelist(b)
    case a => List(g(a))
  }
}
