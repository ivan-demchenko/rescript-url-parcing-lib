open Parser
open Component

@genType
let allVarsStr =
  takeBetween("{{", "}}")->Parser.map(x => x->Variable)->Parser.collect

@genType
let parseProtocol = takeUntil("://")->Parser.map(x => x->Protocol)

@genType
let parseDomain =
  takeUntil("/")
  ->Parser.map(s => s ++ "/")
  ->Parser.alt(takeUntil("#")->Parser.map(s => s ++ "#"))
  ->Parser.alt(Parser.takeRest)
  ->Parser.subParse(
    takeUntil(".")->Parser.alt(takeUntil("/"))->Parser.alt(Parser.takeRest)->Parser.collect,
  )
  ->Parser.map(xs => xs->Domain)

@genType
let parsePath =
  takeUntil("?")
  ->Parser.map(x => x ++ "?")
  ->Parser.alt(takeUntil("#")->Parser.map(x => x ++ "#"))
  ->Parser.alt(Parser.takeRest)
  ->Parser.subParse(
    takeUntil("/")->Parser.alt(takeUntil("#"))->Parser.alt(Parser.takeRest)->Parser.collect,
  )
  ->Parser.map(x => x->Path)

@genType
let parseHash = takeFrom("#")->Parser.map(x => list{Hash(x)})

@genType
let queryString =
  takeUntil("#")
  ->Parser.map(s => s ++ "#")
  ->Parser.alt(Parser.takeRest)
  ->Parser.subParse(
    takeUntil("&")
    ->Parser.alt(takeUntil("#"))
    ->Parser.alt(Parser.takeRest)
    ->Parser.subParse(takeUntil("=")->Parser.alt(Parser.takeRest)->Parser.collect)
    ->Parser.collect,
  )
  ->Parser.map(xs => xs->Query)
