module FSharpCompiler.Tests

open FSharpCompiler.IntermediateLanguage

[<ReflectedDefinition>]
module TestFunctions =
  type Marker = Marker of unit

  let identity (n: int) = n

  let cubic n = n*n*n - n - 1

  let rec factorial n =
    if n<2 then 1 else
      n * factorial(n-1)

  let rec fib n =
    if n<2 then n else
      fib(n-1) + fib(n-2)

let runTests (compile: _ -> ExecutionEngine) =
  let runTest name arg =
    let ty = typeof<TestFunctions.Marker>.DeclaringType
    let methodInfo = ty.GetMethod name
    use fn = compile methodInfo
    let ret = fn.Invoke arg
    printfn "%s(%d) = %d" name arg ret

  runTest "identity" 10
  runTest "cubic" 3
  runTest "factorial" 10
  runTest "fib" 40

do
  let compile methodInfo =
    let ilFunction = Quotations.compile methodInfo
    printfn "ILFunction = %A" ilFunction
    let code = X86CodeGen.compile ilFunction
    printfn "Machine code = %A" code
    new ExecutionEngine(code)
  runTests compile
