open Parser
open Components

let allVarsStr = takeBetween("{{", "}}")->Parser.map(x => x->Variable)->Parser.collect
let parseProtocol = takeUntil("://")->Parser.map(x => x->Protocol)

let parseDomain =
  takeUntil("/")
  ->Parser.map(s => s ++ "/")
  ->Parser.alt(takeUntil("#")->Parser.map(s => s ++ "#"))
  ->Parser.alt(Parser.return(s => Some("", s)))
  ->Parser.subParse(takeUntil(".")->Parser.alt(takeUntil("/"))->Parser.collect)
  ->Parser.map(xs => xs->Domain)

let parsePath =
  takeUntil("?")
  ->Parser.alt(takeUntil("#"))
  ->Parser.alt(Parser.return(s => Some("", s)))
  ->Parser.map(x => x->Path)

let parseHash = takeFrom("#")->Parser.map(x => list{Hash(x)})

let queryString =
  takeUntil("#")
  ->Parser.map(s => s ++ "#")
  ->Parser.subParse(takeUntil("&")->Parser.alt(takeUntil("#"))->Parser.collect)
  ->Parser.map(xs => xs->Query)
