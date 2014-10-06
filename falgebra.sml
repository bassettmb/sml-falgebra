signature FUNCTOR = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

signature FALGEBRA = sig
  type 'a t
  datatype mu = Mu of mu t
  val cata : ('a t -> 'a) -> mu -> 'a
end

functor FAlgebra(F: FUNCTOR) : FALGEBRA = struct
  type 'a t = 'a F.t
  datatype mu = Mu of mu t
  val unfold : mu -> mu t = fn (Mu f) => f
  fun cata (reduce: 'a t -> 'a) (expr: mu) : 'a =
    reduce (F.fmap (cata reduce) (unfold expr))
end

structure Expr = struct

  datatype 'a expr = Const of int
                   | Add of 'a * 'a
                   | Mul of 'a * 'a

  type 'a t = 'a expr

  fun fmap f (Const x) = Const x
    | fmap f (Add (x, y)) = Add (f x, f y)
    | fmap f (Mul (x, y)) = Mul (f x, f y)

end

fun reduce (Expr.Const x) = x
  | reduce (Expr.Add (x, y)) = x + y
  | reduce (Expr.Mul (x, y)) = x * y

structure Alg = FAlgebra(Expr)

val example = (Alg.Mu (Expr.Mul
                (Alg.Mu (Expr.Const 3),
                 Alg.Mu (Expr.Const 4))))

val fixpoint = Alg.cata reduce example
