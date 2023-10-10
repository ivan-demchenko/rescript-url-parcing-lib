open Components

type mode = Protocol | Domain | Path | Query | End

let compToList = maybeComp => maybeComp->Option.map(x => list{x})->Option.getWithDefault(list{})

let rec parse' = (input: list<string>, mode: mode, res: list<component>): list<component> => {
  switch mode {
  | Protocol => {
      let ((comps, vars), rest) = Protocol.parse(input)
      parse'(rest, Domain, list{...comps, ...res, ...vars})
    }
  | Domain => {
      let ((comps, vars), rest) = Domain.parse(input)
      parse'(rest, Path, list{...comps, ...res, ...vars})
    }
  | Path => {
      let ((comps, vars), rest) = Path.parse(input, "", list{}, list{})
      parse'(rest, Query, list{...comps, ...res, ...vars})
    }
  | Query => {
      let ((comps, vars), rest) = QueryString.parse(input)
      parse'(rest, End, list{...comps, ...res, ...vars})
    }
  | End => res
  }
}

let parse = (input: list<string>): list<component> => {
  parse'(input, Protocol, list{})
}

type result = {
  protocol: Js.Array.t<string>,
  domain: Js.Array.t<string>,
  path: Js.Array.t<string>,
  query: Js.Array.t<Js.Array.t<string>>,
  variables: Js.Array.t<string>,
}

let emptyResult: result = {
  protocol: [],
  domain: [],
  path: [],
  query: [],
  variables: [],
}

let run = input => {
  input
  ->Js.String2.split("")
  ->Belt.List.fromArray
  ->parse
  ->Belt.List.reduce(emptyResult, (res, comp) => {
    switch comp {
    | Protocol(x) => {...res, protocol: [x]}
    | Domain(x) => {...res, domain: [x]}
    | Path(xs) => {...res, path: Belt.List.toArray(xs)}
    | Query(xs) => {...res, query: xs->Belt.List.map(pairToArr)->Belt.List.toArray}
    | Variable(x) => {...res, variables: Js.Array.concat(res.variables, [x])}
    }
  })
}

Js.Console.log2("----\n", run("https://domain.com/api/test"))
Js.Console.log2("----\n", run("https://{{baseUrl}}.com/api/{{apiVer}}/test?age=24&bob=john"))
Js.Console.log2(
  "----\n",
  run("https://{{baseUrl}}.com/api/{{apiVer}}/test?age={{age}}&{{nameKey}}={{nameVal}}"),
)
Js.Console.log2("----\n", run("{{baseUrl}}"))
