/// A back-end that compiles our intermediate language into x86
/// machine code.
module FSharpCompiler.X86CodeGen

module X86Asm =
  type condition = A | Ge | Always

  type instruction<'location> =
    | Label of string
    | PushEax
    | PushEbp
    | PopEax
    | PopEbp
    | MovEax of int
    | AddEax of int
    | AddEsp of int
    | AddFromEspToEax
    | MulFromEspToEax
    | MovEbpEsp
    | StoreEaxAtEbpOffset of int
    | StoreEaxAtEspOffset of int
    | LoadEaxFromEbpOffset of int
    | LoadEaxFromEspOffset of int
    | FstpToEspOffset of int
    | FldFromEspOffset of int
    | Fcomip
    | FAddFrom of 'location
    | FSubFrom of 'location
    | FMulFrom of 'location
    | CmpEax of int
    | Jump of condition * 'location
    | Ret
    | Call of 'location

  let size = function
    | Label _ -> 0
    | PushEbp | PushEax | PopEax | PopEbp | Ret -> 1
    | Jump(_, _) | Fcomip | MovEbpEsp -> 2
    | StoreEaxAtEbpOffset _ | AddEsp _ | AddEax _ | AddFromEspToEax | CmpEax _
    | LoadEaxFromEbpOffset _ -> 3
    | FstpToEspOffset _ | FldFromEspOffset _ | StoreEaxAtEspOffset _
    | LoadEaxFromEspOffset _ | MulFromEspToEax -> 4
    | MovEax _ | Call _ -> 5
    | FAddFrom _ | FSubFrom _ | FMulFrom _ -> 6

  open System.Collections.Generic

  let assemble instrs =
    let lbls = Dictionary(HashIdentity.Structural)
    let program = ResizeArray()
    for pass in 1..2 do
      let pc = ref 0
      for instr in instrs do
        let find lbl =
          if pass=1 then 0 else
            lbls.[lbl] - (!pc + size instr)
        let code =
          match instr with
          | PushEbp -> [0x55uy]
          | PushEax -> [0x50uy]
          | PopEbp -> [0x5duy]
          | PopEax -> [0x58uy]
          | MovEbpEsp -> [0x89uy; 0xe5uy]
          | StoreEaxAtEbpOffset n -> [0x89uy; 0x45uy; byte n]
          | StoreEaxAtEspOffset n -> [0x89uy; 0x44uy; 0x24uy; byte n]
          | LoadEaxFromEbpOffset n -> [0x8buy; 0x45uy; byte n]
          | MovEax n ->
              let n = uint32 n
              let f i = (n >>> (8*i)) |> byte
              [0xb8uy; f 0; f 1; f 2; f 3]
          | AddEax n -> [0x83uy; 0xc0uy; byte n]
          | AddEsp n -> [0x83uy; 0xc4uy; byte n]
          | AddFromEspToEax -> [0x03uy; 0x04uy; 0x24uy]
          | MulFromEspToEax -> [0x0fuy; 0xafuy; 0x04uy; 0x24uy]
          | LoadEaxFromEspOffset n -> [0x8buy; 0x44uy; 0x24uy; byte n]
          | FstpToEspOffset n -> [0xdduy; 0x5cuy; 0x24uy; byte n]
          | FldFromEspOffset n -> [0xdduy; 0x44uy; 0x24uy; byte n]
          | Fcomip -> [0xdfuy; 0xf1uy]
          | FAddFrom lbl ->
              let n = find lbl
              let f i = (n >>> (8*i)) |> byte
              [0xdcuy; 0x05uy; f 0; f 1; f 2; f 3]
          | FSubFrom lbl ->
              let n = find lbl
              let f i = (n >>> (8*i)) |> byte
              [0xdcuy; 0x25uy; f 0; f 1; f 2; f 3]
          | FMulFrom lbl ->
              let n = find lbl
              let f i = (n >>> (8*i)) |> byte
              [0xdcuy; 0x0duy; f 0; f 1; f 2; f 3]
          | CmpEax n -> [0x83uy; 0xf8uy; byte n]
          | Jump(Ge, lbl) -> [0x7duy; byte(find lbl)]
          | Jump(Always, lbl) -> [0xebuy; byte(find lbl)]
          | Jump(A, lbl) -> [0x77uy; byte(find lbl)]
          | Ret -> [0xc3uy]
          | Call lbl ->
              let n = find lbl
              let f i = (n >>> (8*i)) |> byte
              [0xe8uy; f 0; f 1; f 2; f 3]
          | Label lbl ->
              lbls.[lbl] <- !pc
              []
        if pass=2 then
          program.AddRange code
        pc := !pc + size instr
    program.ToArray()

  let rec simplify = function
    | PopEax::PushEax::(LoadEaxFromEbpOffset _::_ as xs) -> simplify xs
    | x::xs -> x::simplify xs
    | [] -> []

open X86Asm
open FSharpCompiler.IntermediateLanguage

module X86CodeGenInternal =
  let compileExpr expr =
    let stackFrameSize = ref 0
    let code = ResizeArray()
    let rec compileExpr sp expr =
      match expr with
      | EInt n ->
          code.AddRange[ PushEax; MovEax n ]
          sp + 4
      | EArg ->
          code.AddRange[ PushEax; LoadEaxFromEspOffset(sp + 4) ]
          sp + 4
      | EAdd(EInt n, f) | EAdd(f, EInt n) ->
          let sp = compileExpr sp f
          code.AddRange[AddEax n]
          sp
      | EAdd(f, g) ->
          let sp = compileExpr sp f
          let sp = compileExpr sp g
          code.AddRange[ AddFromEspToEax; AddEsp 4 ]
          sp - 4
      | EMul(f, g) ->
          let sp = compileExpr sp f
          let sp = compileExpr sp g
          code.AddRange[ MulFromEspToEax; AddEsp 4 ]
          sp - 4
      | EApply(f, x) ->
          let sp = compileExpr sp x
          code.AddRange[ Call f ]
          sp
      | EIfLt(x, n, t, f) ->
          let fBlk = sprintf "fBlk%d" (id())
          let kBlk = sprintf "kBlk%d" (id())
          let sp = compileExpr sp x
          code.AddRange[ CmpEax n; Jump(Ge, fBlk); PopEax ]
          let sp1 = compileExpr (sp - 4) t
          code.AddRange[ Jump(Always, kBlk); Label fBlk; PopEax ]
          let sp2 = compileExpr (sp - 4) f
          code.AddRange[ Label kBlk ]
          sp1
      | EReturn f ->
          let sp = compileExpr sp f
          code.AddRange[ AddEsp 4; Ret ]
          sp - 4
    let sp = compileExpr -4 expr
    [ yield! code.ToArray() ]

  let compileFunction fn =
    [ yield! [ LoadEaxFromEspOffset 4; Label fn.Name ]
      yield! EReturn fn.Body |> compileExpr ]

open X86CodeGenInternal

/// Compile IL into x86 machine code.
let compile ilFunction =
  compileFunction ilFunction
  |> simplify
  |> assemble
