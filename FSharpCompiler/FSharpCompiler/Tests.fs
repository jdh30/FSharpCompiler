module FSharpCompiler.Tests

open FSharpCompiler.IntermediateLanguage

[<ReflectedDefinition>]
module TestFunctions =
  type Marker = Marker of unit

  let identity (n: int) = n

  let cubic n = n*n*n - n - 1

  let sign n = if n<0 then -1 else 1

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
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let ret = fn.Invoke arg
    printfn "%s(%d) = %d in %fs" name arg ret timer.Elapsed.TotalSeconds

  //runTest "identity" 10
  //runTest "cubic" 3
  runTest "sign" -13
  runTest "sign" 13
  //runTest "factorial" 10
  //runTest "fib" 40

do
  let compile methodInfo =
    let ilFunction = Quotations.compile methodInfo
    printfn "ILFunction = %A" ilFunction
    let instrs = X86CodeGen.compile ilFunction
    printfn "Assembler = %A" instrs
    let instrs = X86CodeGen.simplify instrs
    printfn "Assembler = %A" instrs
    let code = X86CodeGen.assemble instrs
    //printfn "Machine code = %A" code
    new ExecutionEngine(code)
  runTests compile
