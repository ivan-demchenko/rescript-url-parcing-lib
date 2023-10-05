let rec parseData = (input: list<string>, var: string): (string, list<string>) => {
  switch input {
  | list{} => ("", input)
  | list{"}", "}", ...rest} => Js.String.length(var) == 0 ? ("", input) : (var, rest)
  | list{c, ...rest} => parseData(rest, var ++ c)
  }
}

let parse = (input: list<string>): (string, list<string>) => {
  parseData(input, "")
}
