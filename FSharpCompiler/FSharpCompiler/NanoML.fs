module FSharpCompiler.NanoML

open FSharpCompiler.IntermediateLanguage

module Parser =
  exception ParseError
  
  let alpha = set['A'..'Z'] + set['a'..'z'] + set['_']
  let numeric = set['0'..'9']
  let alphaNumeric = alpha + numeric

  let (|Empty|Char|) (s: string, i) =
    if i < s.Length then Char(s.[i], (s, i+1)) else Empty

  let (|One|_|) alphabet = function
    | Empty -> None
    | s, i when Set.contains s.[i] alphabet -> Some(s, i+1)
    | _ -> None

  let rec (|Star|) alphabet = function
    | One alphabet (Star alphabet it)
    | it -> it

  let (|Plus|_|) alphabet = function
    | One alphabet (Star alphabet it) -> Some it
    | _ -> None

  let sub (s: string, i0) (_, i1) =
    s.Substring(i0, i1-i0)

  type Token =
    | NUM of float
    | CMP of Comparison
    | LPAREN | RPAREN
    | EOF

  let (|Ident|_|) = function
    | One alpha (Star alphaNumeric it) -> Some it
    | _ -> None
(*
  let (|Lex|) it =
    let rec next = function
      | Char((' ' | '\t'), it) -> next it
      | (Plus numeric (Char('.', Plus numeric it2)) as it1)
      | (Plus numeric it2 as it1) -> NUM(float(sub it1 it2)), it2
      | (Plus op it2 | Ident it2) as it1 -> OP(sub it1 it2), it2
      | Char('(', it) -> LPAREN, it
      | Char(')', it) -> RPAREN, it
      | Empty -> EOF, it
      | s, i -> failwithf "Mistake at char %d" i
    next it

  let rec (|ParseLine|) = function
    | Expr(expr, Lex(EOF, _)) -> expr
    | _ -> raise ParseError
  and (|Atom|_|) = function
    | Lex(NUM n, Atom(ns, it)) -> Some(n::ns, it)
    | Lex(NUM n, it) -> Some([n], it)
    | _ -> None
  and (|SimpleExpr|_|) = function
    | Atom(a, it) -> Some(Atom a, it)
    | Lex(LPAREN, Expr(f, Lex(RPAREN, it))) -> Some(Parenthesized f, it)
    | Lex(OP op, Expr(f, it)) -> Some(Unary(op, f), it)
    | Lex(LPAREN, Lex(RPAREN, it)) -> Some(Atom [], it)
    | _ -> None
  and (|Expr|_|) = function
    | SimpleExpr(f, Lex(OP op, Expr(g, it))) -> Some(Binary(f, op, g), it)
    | SimpleExpr(f, it) -> Some(f, it)
    | _ -> None
*)


