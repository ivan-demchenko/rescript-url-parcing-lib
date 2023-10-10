type parsingStep = (string, string)
// parsers
let takeUntil = (em: string, source: string): parsingStep => {
  String.indexOfOpt(source, em)
  ->Option.map(ei => (
    String.substring(source, ~start=0, ~end=ei),
    String.substringToEnd(source, ~start=ei + String.length(em)),
  ))
  ->Option.getWithDefault(("", source))
}

let takeBetween = (sm: string, em: string, source: string): parsingStep => {
  String.indexOfOpt(source, sm)
  ->Option.flatMap(si => {
    String.indexOfOpt(source, em)->Option.map(ei => {
      (
        String.substring(source, ~start=si + String.length(sm), ~end=ei),
        String.substringToEnd(source, ~start=ei + String.length(em)),
      )
    })
  })
  ->Option.getWithDefault(("", source))
}
// components
type cmp = Prot(string) | Var(string) | Dmn(string)
let cmpToStr = (comp): string => {
  switch comp {
  | Prot(n) => `Protocol(${n})`
  | Var(n) => `Var(${n})`
  | Dmn(n) => `Domain(${n})`
  }
}
type parsingRes = (list<cmp>, string)

let protocol = (parser: string => (string, string), source: string): parsingRes => {
  switch parser(source) {
  | ("", rest) => (list{}, rest)
  | (x, rest) => (list{Prot(x)}, rest)
  }
}

let variable = (parser: string => (string, string), source: string): parsingRes => {
  switch parser(source) {
  | ("", rest) => (list{}, rest)
  | (x, rest) => (list{Var(x)}, rest)
  }
}

let domain = (parser: string => (string, string), source: string): parsingRes => {
  switch parser(source) {
  | ("", rest) => (list{}, rest)
  | (x, rest) => (list{Dmn(x)}, rest)
  }
}

// combinators
let either = (op1: string => parsingRes, op2: string => parsingRes, source: string): parsingRes => {
  switch op1(source) {
  | (list{}, _) => op2(source)
  | (comp, rest) => (comp, rest)
  }
}

let rec takeAll = (p: string => parsingRes, acc: list<cmp>, source: string): parsingRes => {
  switch source {
  | "" => (acc, "")
  | val => {
      let (xs, rest) = p(val)
      switch xs {
      | list{} => (acc, rest)
      | _ => takeAll(p, Belt.List.concat(acc, xs), rest)
      }
    }
  }
}

let parseWithin = (
  main: string => parsingRes,
  extras: string => parsingRes,
  source: string,
): parsingRes => {
  let (x1, rest) = main(source)
  let (x2, _) = extras(source)
  (list{...x1, ...x2}, rest)
}

let runAll = (fns: list<string => parsingRes>, source: string): parsingRes => {
  fns->Belt.List.reduce((list{}, source), (res, fn) => {
    let (c1, toParse) = res
    let (c2, rest) = fn(toParse)
    (list{...c1, ...c2}, rest)
  })
}

// test
let dbg = pair => {
  let (cmps, rest) = pair
  let xs = Belt.List.map(cmps, cmpToStr)->List.toArray->Array.joinWith(", ")
  `(${xs}, "${rest}")`
}

let parseVar = variable(takeBetween("{{", "}}"))
let parseProtocol = either(protocol(takeUntil("://")), parseVar)
let parseDomain = parseWithin(domain(takeUntil("/")), takeAll(parseVar, list{}))

let x = runAll(
  list{parseProtocol, parseDomain},
  "https://{{v1}}.google.{{v2}}.com/api/test?q=abc&w=12#hash",
)

Js.Console.log(dbg(x))
