/// A front-end that compiles reflected F# definitions into our
/// intermediate language.
module FSharpCompiler.Quotations

open Microsoft.FSharp.Quotations
open FSharpCompiler.IntermediateLanguage

let rec compileExpr = function
  | Patterns.Lambda(p, body) -> compileExpr body
  | Patterns.Var _ -> EArg
  | x -> failwithf "Not yet supported: %A" x

/// Compile a reflected definition in IL.
let compile (methodInfo: System.Reflection.MethodInfo) =
  {
    Name = methodInfo.Name
    ArgumentType = TInt
    ReturnType = TInt
    Body =
      match Quotations.Expr.TryGetReflectedDefinition methodInfo with
      | Some body -> compileExpr body
      | None -> failwith "No reflected definition for this function"
  }
