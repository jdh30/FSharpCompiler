/// A front-end that compiles reflected F# definitions into our
/// intermediate language.
module FSharpCompiler.Quotations

open Microsoft.FSharp.Quotations
open FSharpCompiler.IntermediateLanguage

/// Compile a reflected definition in IL.
let compile (methodInfo: System.Reflection.MethodInfo) =
  let (|Name|) (methodInfo: System.Reflection.MethodInfo) =
    Name methodInfo.Name
  let rec compileExpr = function
    | Patterns.Value(:? int as n, _) -> EInt n
    | Patterns.Lambda(p, body) -> compileExpr body
    | Patterns.Var _ -> EArg
    | Patterns.Call(None, Name "op_Addition", [f; g]) ->
        compileExpr f + compileExpr g
    | Patterns.Call(None, Name "op_Subtraction", [f; g]) ->
        compileExpr f - compileExpr g
    | Patterns.Call(None, Name "op_Multiply", [f; g]) ->
        compileExpr f * compileExpr g

    | Patterns.Call(None, Name "op_LessThan", [f; g]) ->
        ECmp(compileExpr f, Lt, compileExpr g)
    | Patterns.Call(None, Name "op_Equality", [f; g]) ->
        ECmp(compileExpr f, Eq, compileExpr g)
    | Patterns.Call(None, Name "op_GreaterThan", [f; g]) ->
        ECmp(compileExpr f, Gt, compileExpr g)

    | Patterns.Call(None, Name "op_LessThan", [f; g]) ->
        EIfLt(compileExpr f - compileExpr g, 0, EInt -1, EInt 0)
    | Patterns.Call(None, Name "op_Equals", [f; g]) ->
        EIf(compileExpr f - compileExpr g, EInt -1, EInt 0)
    | Patterns.Call(None, Name "op_GreaterThan", [f; g]) ->
        EIfLt(compileExpr g - compileExpr f, 0, EInt -1, EInt 0)

    | Patterns.IfThenElse(p, t, f) ->
        EIf(compileExpr p, compileExpr t, compileExpr f)
    | Patterns.Call(None, Name name, [f]) when name = methodInfo.Name ->
        EApply(name, compileExpr f)
    | x -> failwithf "Not yet supported: %A" x
  {
    Name = methodInfo.Name
    ArgumentType = TInt
    ReturnType = TInt
    Body =
      match Quotations.Expr.TryGetReflectedDefinition methodInfo with
      | Some body -> compileExpr body
      | None -> failwith "No reflected definition for this function"
  }
