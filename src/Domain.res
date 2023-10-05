open Components

let rec parse' = (input: list<string>, acc: string, vars: list<string>): (
  (list<component>, list<component>),
  list<string>,
) => {
  switch input {
  | list{} => ((list{Domain("")}, strsToVars(vars)), input)
  | list{"/", ...rest} => ((list{Domain(acc)}, strsToVars(vars)), rest)
  | list{"{", "{", ...rest} => {
      let (var, rest) = Variables.parse(rest)
      parse'(rest, `${acc}{{${var}}}`, Belt.List.add(vars, var))
    }
  | list{c, ...rest} => parse'(rest, acc ++ c, vars)
  }
}

let parse = (input: list<string>): ((list<component>, list<component>), list<string>) => {
  parse'(input, "", list{})
}
