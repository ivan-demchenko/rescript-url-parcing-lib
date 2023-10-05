open Components

let rec parse = (input: list<string>, acc: string, paths: list<string>, vars: list<string>): (
  (list<component>, list<component>),
  list<string>,
) => {
  switch input {
  | list{} => ((list{Path(paths)}, strsToVars(vars)), input)
  | list{"?", ...rest} => ((list{Path(paths)}, strsToVars(vars)), rest)
  | list{"/", ...rest} =>
    parse(rest, "", Js.String.length(acc) == 0 ? paths : Belt.List.add(paths, acc), vars)
  | list{"{", "{", ...rest} => {
      let (var, rest) = Variables.parse(rest)
      let vars' = Belt.List.add(vars, var)
      let paths' = Belt.List.add(paths, `{{${var}}}`)
      parse(rest, "", paths', vars')
    }
  | list{c, ...rest} => parse(rest, acc ++ c, paths, vars)
  }
}
