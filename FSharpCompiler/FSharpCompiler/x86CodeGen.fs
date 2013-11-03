/// A back-end that compiles our intermediate language into x86
/// machine code.
module FSharpCompiler.X86CodeGen

module X86Asm =
  type Condition =
    /// Equal to.
    | Eq

    /// Above (unsigned >=).
    | A

    /// Greater than or equal to (signed >=).
    | Ge

    /// Greater than (signed >).
    | Gt

    /// Unconditional.
    | Always

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
    | Jump of Condition * 'location
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
          | Jump(cmp, lbl) ->
              let op =
                match cmp with
                | Eq -> 0x74uy
                //| Ne -> 0x75uy
                | Ge -> 0x7duy
                | Gt -> 0x7fuy
                | A -> 0x77uy
                | Always -> 0xebuy
              [op; byte(find lbl)]
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
    let emit sp instrs =
      let mutable sp = sp
      for instr in instrs do
        code.Add instr
        match instr with
        | PushEax
        | PushEbp ->
            // Push onto the stack displaces the frame pointer by 4.
            sp <- sp + 4
        | PopEax
        | PopEbp
        | AddFromEspToEax
        | MulFromEspToEax ->
            // Popping off the stack displaces the frame pointer by 4.
            sp <- sp - 4
        | Label _
        | MovEax _
        | AddEax _
        | AddEsp _
        | MovEbpEsp
        | StoreEaxAtEbpOffset _
        | StoreEaxAtEspOffset _
        | LoadEaxFromEbpOffset _
        | LoadEaxFromEspOffset _
        | FstpToEspOffset _
        | FldFromEspOffset _
        | Fcomip
        | FAddFrom _
        | FSubFrom _
        | FMulFrom _
        | CmpEax _
        | Jump _
        | Ret
        | Call _ -> ()
      sp
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
      | ECmp(f, op, g) ->
          let tBlk = sprintf "tBlk%d" (id())
          let kBlk = sprintf "kBlk%d" (id())
          let sp = compileExpr sp (f - g)
          let cond, tn, fn =
            match op with
            | Comparison.Lt -> Condition.Gt, 0, -1
            | Comparison.Le -> Condition.Ge, 0, -1
            | Comparison.Eq -> Condition.Eq, -1, 0
            | Comparison.Ne -> Condition.Eq, 0, -1
            | Comparison.Ge -> Condition.Ge, -1, 0
            | Comparison.Gt -> Condition.Gt, -1, 0
          code.AddRange[ CmpEax 0; Jump(cond, tBlk) ]
          code.AddRange[ PopEax; MovEax fn; Jump(Always, kBlk) ]
          code.AddRange[ Label tBlk; PopEax; MovEax tn ]
          code.AddRange[ Label kBlk ]
          sp
      | EIf(p, t, f) ->
          let fBlk = sprintf "fBlk%d" (id())
          let kBlk = sprintf "kBlk%d" (id())
          let sp = compileExpr sp p
          code.AddRange[ CmpEax 0; Jump(Condition.Eq, fBlk); PopEax ]
          let sp1 = compileExpr (sp - 4) t
          code.AddRange[ Jump(Always, kBlk); Label fBlk; PopEax ]
          let sp2 = compileExpr (sp - 4) f
          code.AddRange[ Label kBlk ]
          sp1
      | EIfLt(x, n, t, f) ->
          let fBlk = sprintf "fBlk%d" (id())
          let kBlk = sprintf "kBlk%d" (id())
          let sp = compileExpr sp x
          code.AddRange[ CmpEax n; Jump(Condition.Ge, fBlk); PopEax ]
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

open X86CodeGenInternal

/// Assemble x86 assembler instructions into machine code.
let assemble = assemble

let simplify = simplify

/// Compile IL into x86 assembler instructions.
let compile fn =
  [ yield! [ LoadEaxFromEspOffset 4; Label fn.Name ]
    yield! EReturn fn.Body |> compileExpr ]
