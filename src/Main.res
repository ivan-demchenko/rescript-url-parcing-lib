open Parsers
open Components

let parser = Parser.parallel(list{
  allVarsStr,
  list{parseProtocol, parseDomain, parsePath, queryString}->Parser.tryAll,
  parseHash,
})

type resultRec = {
  protocol: option<string>,
  domain: option<array<string>>,
  path: option<string>,
  query: option<array<string>>,
  hash: option<string>,
  variables: option<array<string>>,
}

let initial: resultRec = {
  protocol: None,
  domain: None,
  path: None,
  query: None,
  hash: None,
  variables: None,
}

let toResults = (xs: list<component>): resultRec =>
  xs->List.reduce(initial, (acc, cmp) => {
    switch cmp {
    | Protocol(x) => {...acc, protocol: Some(x)}
    | Domain(xs) => {...acc, domain: Some(xs->List.toArray)}
    | Path(x) => {...acc, path: Some(x)}
    | Query(xs) => {...acc, query: Some(xs->List.toArray)}
    | Hash(x) => {...acc, hash: Some(x)}
    | Variable(x) => {
        ...acc,
        variables: acc.variables->Option.map(xs => Array.concat(xs, [x]))->Option.orElse(Some([x])),
      }
    }
  })

let input = "{{prot}}://{{v1}}.google.{{v2}}.com/api/{{api-version}}"

let parsingRes =
  parser
  ->Parser.runParser(input)
  ->Option.map(((_, xs)) => {
    xs->List.flatten->toResults
  })
  ->Option.getWithDefault(initial)

Js.Console.log(parsingRes)
