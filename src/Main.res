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
      let (maybeComp, rest) = QueryString.parse(input)
      rest->parse'(End, maybeComp->compToList->Belt.List.concat(res))
    }
  | End => res
  }
}

let parse = (input: list<string>): list<component> => {
  parse'(input, Protocol, list{})
}

let run = input => {
  input
  ->Js.String2.split("")
  ->Belt.List.fromArray
  ->parse
  ->Belt.List.map(componentToStr)
  ->Belt.List.toArray
  ->Js.Array2.joinWith(", ")
}

Js.Console.log2("1:\n", run("https://domain.com/api/test"))
Js.Console.log2("2:\n", run("https://{{baseUrl}}.com/api/{{apiVer}}/test?age=24&bob=john"))
Js.Console.log2("3:\n", run("{{baseUrl}}"))
