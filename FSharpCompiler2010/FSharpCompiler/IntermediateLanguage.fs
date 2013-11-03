/// Our intermediate language, common to the code generators for
/// all architectures.
module FSharpCompiler.IntermediateLanguage

(*
We use an unusually-simple intermediate language. There are no
loops. All iteration is done via recursion. This will simplify
the garbage collector (see HLVM).
*)

type Type =
  | TInt

  /// Get the size of a value of this type in bytes.
  member ty.Size =
    match ty with
    | TInt -> 4

type Expr =
  | EReturn of Expr
  | EInt of int
  | EArg
  | EAdd of Expr * Expr
  | EMul of Expr * Expr
  | EIfLt of Expr * int * Expr * Expr
  | EApply of string * Expr

  static member (+) (f, g) =
    match f, g with
    | EInt m, EInt n -> EInt(m+n)
    | f, EAdd(g, h) -> f + g + h
    | EAdd(f, g), EInt n -> EInt n + f + g
    | f, EInt n -> EInt n + f
    | f, g -> EAdd(f, g)

  static member (-) (f, g) = f + EInt -1 * g

  static member (*) (f, g) =
    match f, g with
    | EInt m, EInt n -> EInt(m*n)
    | f, EMul(g, h) -> f * g * h
    | EMul(f, g), EInt n -> EInt n * f * g
    | f, EInt n -> EInt n * f
    | f, g -> EMul(f, g)

let id =
  let n = ref 0
  fun () ->
    incr n
    !n

type Function = {
  Name: string
  ArgumentType: Type
  ReturnType: Type
  Body: Expr
}
