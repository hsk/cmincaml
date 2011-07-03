/*
 13 bit即値最適化(simm13.ml)
 
 SPARCアセンブリでは、ほとんどの整数演算の第二オペランドとして、レジスタだけでなく13 bit以下（-4096以上4096未満）の整数即値をとることができます。そのための最適化処理をSimm13.gおよびSimm13.g'で実装しています。対象が仮想SPARCアセンブリであり、定数が13 bit整数に制限されていること以外は、定数畳み込みや不要定義削除とほぼ同様です。
 
 */
package mincaml;
import scala.collection.immutable._;
import mincaml.X86Asm._;
object Simm13 {

  // 命令列の13bit即値最適化 (caml2html: simm13_g)
  def g(env:Map[Id.T,Int], e:T):T = e match {
    case Ans(exp) => Ans(gdash(env, exp))
    case Let((x, t), SET(i), e) if ((-4096 <= i) && (i < 4096)) =>
      // println("found simm13 " + x + " = " + i + "@.")
      val edash = g(env + (x -> i), e);
      if (fv(edash).contains(x)) {
        Let((x, t), SET(i), edash)
      } else {
        println("erased redundant Set to "+x+"@.");
        edash
      }
    case Let(xt, SLL(y, C(i)), e) if (env.contains(y)) => // for array access
      // println("erased redundant SLL on " + x + "@.");
      g(env, Let(xt, SET(env(y) << i), e))
    case Let(xt, exp, e) => Let(xt, gdash(env, exp), g(env, e))
    case Forget(x, e) => Forget(x, g(env, e))
  }

  // 各命令の13bit即値最適化 (caml2html: simm13_gprime)
  def gdash(env:Map[Id.T,Int], e:Exp):Exp = e match {
    case Add(x, V(y)) if (env.contains(y)) => Add(x, C(env(y)))
    case Add(x, V(y)) if (env.contains(x)) => Add(y, C(env(x)))
    case Sub(x, V(y)) if (env.contains(y)) => Sub(x, C(env(y)))
    case SLL(x, V(y)) if (env.contains(y)) => SLL(x, C(env(y)))
    case Ld(x, V(y)) if (env.contains(y)) => Ld(x, C(env(y)))
    case St(x, y, V(z)) if (env.contains(z)) => St(x, y, C(env(z)))
    case LdDF(x, V(y)) if (env.contains(y)) => LdDF(x, C(env(y)))
    case StDF(x, y, V(z)) if (env.contains(z)) => StDF(x, y, C(env(z)))
    case IfEq(x, V(y), e1, e2) if (env.contains(y)) => IfEq(x, C(env(y)), g(env, e1), g(env, e2))
    case IfLE(x, V(y), e1, e2) if (env.contains(y)) => IfLE(x, C(env(y)), g(env, e1), g(env, e2))
    case IfGE(x, V(y), e1, e2) if (env.contains(y)) => IfGE(x, C(env(y)), g(env, e1), g(env, e2))
    case IfEq(x, V(y), e1, e2) if (env.contains(x)) => IfEq(y, C(env(x)), g(env, e1), g(env, e2))
    case IfLE(x, V(y), e1, e2) if (env.contains(x)) => IfGE(y, C(env(x)), g(env, e1), g(env, e2))
    case IfGE(x, V(y), e1, e2) if (env.contains(x)) => IfLE(y, C(env(x)), g(env, e1), g(env, e2))
    case IfEq(x, ydash, e1, e2) => IfEq(x, ydash, g(env, e1), g(env, e2))
    case IfLE(x, ydash, e1, e2) => IfLE(x, ydash, g(env, e1), g(env, e2))
    case IfGE(x, ydash, e1, e2) => IfGE(x, ydash, g(env, e1), g(env, e2))
    case IfFEq(x, y, e1, e2) => IfFEq(x, y, g(env, e1), g(env, e2))
    case IfFLE(x, y, e1, e2) => IfFLE(x, y, g(env, e1), g(env, e2))
    case e => e
  }

  // トップレベル関数の13bit即値最適化
  def h(e:Fundef):Fundef = e match {
    case Fundef(l, xs, ys, e, t) => Fundef(l, xs, ys, g(Map[Id.T,Int](), e), t)
  }

  // プログラム全体の13bit即値最適化
  def f1 (e:Prog):Prog = e match {
    case Prog(data, fundefs, e) => Prog(data, fundefs.map(h) , g(Map[Id.T,Int](), e))
  }
	
  def apply(e:X86Asm.Prog):X86Asm.Prog = {
    f1(e)
  }
}
