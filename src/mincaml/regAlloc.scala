/*
 レジスタ割り当て(regAlloc.ml)
 
 [2005年11月3日追加: ソースコードに含まれている三つのファイルregAlloc.notarget-nospill.ml, regAlloc.target-nospill.ml, regAlloc.target-latespill.mlでは、spillingやtargetingを省略ないし簡単にしたレジスタ割り当ても実装されています。インターフェースは同じなので、regAlloc.mlのかわりに使うことができます。]
 
 [2008年6月19日追加: 「コンピュータソフトウェア」誌25巻2号28～38頁（2008年4月）に掲載された論文「MinCamlコンパイラ」で解説されているレジスタ割り当ては、regAlloc.target-latespill.mlに相当し、以下の「さかのぼってSaveを挿入する」処理（ToSpillやNoSpillなど）が省略されています。現在では、従来のregAlloc.ml (regAlloc.target-earlyspill.ml)のかわりに、このregAlloc.target-latespill.mlを使用することを推奨しています。実装がより単純で、レジスタ割り当て自体が高速になり、コンパイルされたプログラムの性能はほとんど変化しないためです。]
 
 MinCamlコンパイラでもっとも複雑な処理が、無限個の変数を有限個のレジスタで実現するレジスタ割り当てです。
 
 まず関数呼び出し規約として、引数は番号の小さいレジスタから順に割り当てていくことにします（レジスタで渡しきれないほど数の多い引数は、プログラマが組などで置き換えることとし、MinCamlコンパイラではサポートしません）。返値は第一レジスタにセットすることにします。これらは関数のレジスタ割り当てRegAlloc.hで処理されます。
 
 その上で、関数の本体やメインルーチンについてレジスタ割り当てをしていきます。RegAlloc.gは、今のレジスタ割り当てを表す（変数からレジスタへの）写像regenvと、命令列とを受け取り、レジスタ割り当てを行って返します。レジスタ割り当ての基本方針は、「まだ生きている（これから使われる）変数が割り当てられているレジスタは避ける」ことです。「まだ生きている変数」はSparcAsm.fvにより計算されます。ただし、Let(x, e1, e2)のe1をレジスタ割り当てする際には、現在の式e1だけでなく、その「後」にくる命令列e2も「これから使われる変数」の計算に含めなければいけません。そのためにRegAlloc.gや、式をレジスタ割り当てする関数RegAlloc.g'では、「これから後」にくる命令列contも引数として受け取り、生きている変数の計算に用いています。
 
 しかし、変数は無限にあってもレジスタは有限なので、どうしても生きているレジスタしか割り当てられないこともあります。その場合は、現在のレジスタの値をメモリに退避する必要があります。これをレジスタ溢れ(register spilling)といいます。命令型言語と違って、関数型言語では変数の値が後から変わることはないので、どうせ退避するならできるだけ早く行うのが得策です。それだけ早くレジスタに空きができるからです。
 
 そこでRegAlloc.gでは、変数xの退避が必要になったら、そのことを表すデータ型ToSpillを返し、xの定義までさかのぼって退避命令Saveを挿入します。また、退避が必要になった時点でxは「生きている変数」から除外したいので、その部分にForgetという仮想命令を挿入して自由変数の集合から除外します。そのためにToSpillは退避すべき変数（のリスト）だけでなく、Forget挿入後の命令列eも保持しています。xを Saveした後はeに対してレジスタ割り当てをやり直すわけです。
 
 退避が必要になるのは、レジスタが不足した場合だけではなく、関数呼び出しもあります。MinCamlではいわゆるcaller saveの慣習を採用しており、関数を呼び出したらレジスタの値は失われます。したがって、その時点で生きている変数の値は、すべて退避する必要があります。ToSpillが一個ではなく複数の変数を（リストとして）保持しているのは、このためです。
 
 なお、もし退避が必要にならなかった場合は、レジスタ割り当てされた命令列e'と、新しいregenvをデータ型NoSpillとして返します。
 
 退避された変数は、いずれまた使用されます。しかしregenvを引いてもレジスタが見つからないので、RegAlloc.g'（式をレジスタ割り当てする関数）で例外が発生します。この例外は関数RegAlloc.g'_and_restoreにより処理され、メモリからレジスタへ変数を復帰する仮想命令Restoreが挿入されます。
 
 レジスタを割り当てるときは、単に「生きているレジスタを避ける」だけでなく、「将来の無駄なmovを削減する」努力もしています（register targetingないしregister coalescingといいます）。たとえば、定義された変数が関数呼び出しの第二引数になるのであれば、できるだけ第二レジスタに割り当てるようにします。また、関数の返値となる変数は、できるだけ第一レジスタに割り当てるようにします。これらを実装したのがRegAlloc.targetです。この処理のためにRegAlloc.gやRegAlloc.g'は、計算結果（関数の返値）を格納するレジスタdestも引数として要求・使用しています。

 */
package mincaml;
import scala.collection.immutable._;
import mincaml.X86Asm._;

object RegAlloc {
  type Selt = Id.T;
	
  // allocate a register or fail
  // val alloc : X86Asm.t -> S.elt M.t -> M.key -> Type.t -> M.key = <fun>
  def alloc(cont:T, regenv:Map[Id.T,Selt], x:Id.T, t:Type.T):Id.T = {
    if (regenv.contains(x)) {
      System.out.println("regenv="+regenv);
      throw new Exception();
    }
    val all = t match {
      case Type.Unit()  => List("%g0") // dummy
      case Type.Float() => allfregs
      case _            => allregs
    }

    if (all == List("%g0")) {
      "%g0"
    } else // [XX] ad hoc optimization
      if (is_reg(x)) {
        x
      } else {
        val free:List[Id.T] = fv(cont);
        try {
          val live = // 生きているレジスタ
            free.foldLeft(List[Id.T]()){
              case (live, y) =>
                if (is_reg(y)) {
                  y :: live
                } else {
                  try {
                    regenv(y) :: live
                  } catch {
                    case _ => live
                  }
                }
            };
          val r = // そうでないレジスタを探す
            all.find{case r => !live.contains(r)};
          // println("allocated " + x + " to " + r + "@.");
          r match {
            case Some(r) => r
            case None => throw new Exception()
          }
        } catch {
          case _ => throw new Exception("register allocation failed for " + x)
        }
      }
  }

  // auxiliary function for g and gdash_and_restore
  // val add : Id.t -> Id.t -> Id.t M.t -> Id.t M.t =
  def add(x:Id.T, r:Id.T, regenv:Map[Id.T,Id.T]):Map[Id.T,Id.T] = {
    if (is_reg(x)) {
      if (x != r) throw new Exception("x == r x="+x+" r="+r);
      regenv
    } else {
      regenv + (x -> r)
    }
  }

  // auxiliary functions for gdash
  case class NoReg(a:Id.T, b:Type.T) extends Exception;
  // val find : Id.t -> Type.t -> Id.t M.t -> Id.t = <fun>
  def find(x:Id.T, t:Type.T, regenv:Map[Id.T,Id.T]):Id.T = {
    if (is_reg(x)) {
      x
    }else {
      try {
        regenv(x)
      } catch {
        case _ => throw new NoReg(x, t)
      }
    }
  }

  // val find' : X86Asm.id_or_imm -> Id.t M.t -> X86Asm.id_or_imm
  def finddash(xdash:id_or_imm, regenv:Map[Id.T,Id.T]):id_or_imm = xdash match {
    case V(x) => V(find(x, Type.Int(), regenv) )
    case c => c
  }

  // Id.t * Type.t -> X86Asm.t -> Id.t M.t -> X86Asm.t -> X86Asm.t * S.elt M.t
  // 命令列のレジスタ割り当て (caml2html: regalloc_g)
  def g(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], e:T):(T,Map[Id.T,Selt]) = e match {
    case Ans(exp) => gdash_and_restore(dest, cont, regenv, exp);
    case Let(xt@(x, t), exp, e) =>
      if(regenv.contains(x))throw new Exception();
      val contdash = concat(e, dest, cont);
      println("contdash="+contdash);
      val (e1dash, regenv1) = gdash_and_restore(xt, contdash, regenv, exp);
      println("call alloc "+contdash+" "+regenv1);
      val r = alloc(contdash, regenv1, x, t);
      val (e2dash, regenv2) = g(dest, cont, add(x, r, regenv1), e);
      (concat(e1dash, (r, t), e2dash), regenv2)
    case Forget(x, e) => throw new Exception()
  }
  // val g'_and_restore :
  //  Id.t * Type.t -> X86Asm.t -> Id.t M.t -> X86Asm.exp -> X86Asm.t * S.elt M.t
  // 使用される変数をスタックからレジスタへRestore (caml2html: regalloc_unspill)
  def gdash_and_restore(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], exp:Exp):(T, Map[Id.T,Selt]) = {
    try {
      println("call gdash "+dest, cont, regenv, exp)
      // regenvを検索
      gdash(dest, cont, regenv, exp)
    } catch {
      case NoReg(x, t) =>
        // みつからない
        println("restoring " + x + "@.")
        g(dest, cont, regenv, Let((x, t), Restore(x), Ans(exp)))
    }
  }

  //  Id.t * Type.t -> X86Asm.t -> Id.t M.t -> X86Asm.exp -> X86Asm.t * S.elt M.t =
  // 各命令のレジスタ割り当て (caml2html: regalloc_gprime)
  def gdash(dest:(Id.T,Type.T), cont:T, regenv:Map[Id.T, Id.T], e:Exp):(T,Map[Id.T, Selt]) = e match {
    case Nop() | SET(_) | SETL(_) | Comment(_) | Restore(_) => (Ans(e), regenv)
    case Mov(x) => (Ans(Mov(find(x, Type.Int(), regenv))), regenv)
    case Neg(x) => (Ans(Neg(find(x, Type.Int(), regenv))), regenv)
    case Add(x, ydash) => (Ans(Add(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
    case Sub(x, ydash) => (Ans(Sub(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
    case SLL(x, ydash) => (Ans(SLL(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
    case Ld (x, ydash) => (Ans(Ld (find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
    case St(x, y, zdash) => (Ans(St(find(x, Type.Int(), regenv), find(y, Type.Int(), regenv), finddash(zdash, regenv))), regenv)
    case FMovD(x) => (Ans(FMovD(find(x, Type.Float(), regenv))), regenv)
    case FNegD(x) => (Ans(FNegD(find(x, Type.Float(), regenv))), regenv)
    case FAddD(x, y) => (Ans(FAddD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
    case FSubD(x, y) => (Ans(FSubD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
    case FMulD(x, y) => (Ans(FMulD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
    case FDivD(x, y) => (Ans(FDivD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
    case LdDF(x, ydash) => (Ans(LdDF(find(x, Type.Int(), regenv), finddash(ydash, regenv))), regenv)
    case StDF(x, y, zdash) => (Ans(StDF(find(x, Type.Float(), regenv), find(y, Type.Int(), regenv), finddash(zdash, regenv))), regenv)
    case exp@IfEq(x, ydash, e1, e2) => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfEq (find(x, Type.Int(),   regenv), finddash(ydash, regenv), e1dash, e2dash), e1, e2)
    case exp@IfLE(x, ydash, e1, e2) => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfLE (find(x, Type.Int(),   regenv), finddash(ydash, regenv), e1dash, e2dash), e1, e2)
    case exp@IfGE(x, ydash, e1, e2) => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfGE (find(x, Type.Int(),   regenv), finddash(ydash, regenv), e1dash, e2dash), e1, e2)
    case exp@IfFEq(x, y, e1, e2)    => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfFEq(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv), e1dash, e2dash), e1, e2)
    case exp@IfFLE(x, y, e1, e2)    => gdash_if(dest, cont, regenv, exp, (e1dash, e2dash) => IfFLE(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv), e1dash, e2dash), e1, e2)
    case exp@CallCls(x, ys, zs)   => gdash_call(dest, cont, regenv, exp, (ys, zs) => CallCls(find(x, Type.Int(), regenv), ys, zs), ys, zs)
    case exp@CallDir(l, ys, zs)   => gdash_call(dest, cont, regenv, exp, (ys, zs) => CallDir(l, ys, zs), ys, zs)
    case Save(x, y) => throw new Exception()
  }

  //  Id.t * Type.t ->
  //  X86Asm.t ->
  //  Id.t M.t ->
  //  X86Asm.exp ->
  //  (X86Asm.t -> X86Asm.t -> X86Asm.exp) ->
  //  X86Asm.t -> X86Asm.t -> X86Asm.t * S.elt M.t = <fun>
  
  // ifのレジスタ割り当て (caml2html: regalloc_if)
  def gdash_if(
    dest:(Id.T, Type.T),
    cont:T,
    regenv:Map[Id.T,Selt],
    exp:Exp,
    constr:(T,T)=>Exp,
    e1:T, e2:T):(T, Map[Id.T,Selt]) = {
    val (e1dash, regenv1) = g(dest, cont, regenv, e1);
    val (e2dash, regenv2) = g(dest, cont, regenv, e2);
		
    // 両方に共通のレジスタ変数だけ利用
    val regenvdash = fv(cont).foldLeft(Map[Id.T, Selt]()){
      case (regenvdash, x) =>
        try {
          if (is_reg(x)) {
            regenvdash
          } else {
            val r1 = regenv1(x);
            val r2 = regenv2(x);
            if (r1 != r2) {
              regenvdash
            } else {
              regenvdash + (x -> r1)
            }
          }
        } catch {
          case _ => regenvdash
        }
    };
		
    (
      fv(cont).foldLeft(Ans(constr(e1dash, e2dash)).asInstanceOf[T]) {
        case (e, x) =>
          if (x == (dest match {case(a,_)=>a}) || !regenv.contains(x) || regenvdash.contains(x)) {
            e
          } else {
            seq(Save(regenv(x), x), e)
          }
      }, // そうでない変数は分岐直前にセーブ
      regenvdash
    )
  }

  //  Id.t * Type.t ->
  //  X86Asm.t ->
  //  Id.t M.t ->
  //  X86Asm.exp ->
  //  (Id.t list -> Id.t list -> X86Asm.exp) ->
  //  Id.t list -> Id.t list -> X86Asm.t * S.elt M.t = <fun>
  // 関数呼び出しのレジスタ割り当て (caml2html: regalloc_call)
  def gdash_call(
    dest:(Id.T, Type.T),
    cont:T,
    regenv:Map[Id.T,Id.T],
    exp:Exp,
    constr:(List[Id.T],List[Id.T])=>Exp,
    ys:List[Id.T],
    zs:List[Id.T]
  ):(T,Map[Id.T,Selt]) = {
    (
      fv(cont).foldLeft(
        Ans(constr(
            ys.map{case y => find(y, Type.Int(),   regenv)},
            zs.map{case z => find(z, Type.Float(), regenv)}
          )).asInstanceOf[T]
      ) {
        case (e, x) =>
          if (x == (dest match {case (a,_)=>a}) || !regenv.contains(x)) {
            e
          } else {
            seq(Save(regenv(x), x), e)
          }
      },
      Map[Id.T,Selt]()
    )
  }

  // X86Asm.fundef -> X86Asm.fundef
  // 関数のレジスタ割り当て (caml2html: regalloc_h)
  def h(f:Fundef):Fundef = f match {
    case Fundef(x, ys, zs, e, t) =>
      val regenv0 = Map(x -> reg_cl);
      val (i, arg_regs, regenv1) = ys.foldLeft((0, List[Id.T](), regenv0)) {
        case ((i, arg_regs, regenv), y) =>
          val r = regs(i);
          (
            i + 1,
            arg_regs ::: List(r),
            { if(is_reg(y))throw new Exception(); regenv + (y -> r) }
          )
      };

      val (d, farg_regs, regenv2) = zs.foldLeft ((0, List[Id.T](), regenv1)) {
        case ((d, farg_regs, regenv), z) =>
          val fr = fregs(d);
          (d + 1, farg_regs ::: List(fr), { if(is_reg(z))throw new Exception(); regenv + (z -> fr) } )
      }
      val a:Id.T = t match {
        case Type.Unit() => Id.gentmp(Type.Unit())
        case Type.Float() => fregs(0)
        case _ => regs(0)
      };
      val (edash:T, regenvdash) = g((a, t), Ans(Mov(a)), regenv2, e);
			
      Fundef(x, arg_regs, farg_regs, edash, t)
  }

  // プログラム全体のレジスタ割り当て (caml2html: regalloc_f)
  def f1(e:Prog):Prog = e match {
    case Prog(data, fundefs, e) =>
      println("register allocation: may take some time (up to a few minutes, depending on the size of functions)@.");
      println("fundefs=" + fundefs);
      val fundefsdash = fundefs.map(h);
      println("fundef ok");
      val (edash, regenvdash) = g((Id.gentmp(Type.Unit()), Type.Unit()), (Ans(Nop())), Map(), e);
      Prog(data, fundefsdash, edash)
  }

  def apply(e:X86Asm.Prog):X86Asm.Prog = f1(e)
}
