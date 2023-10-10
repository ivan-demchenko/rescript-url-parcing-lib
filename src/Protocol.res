open Components

let rec parse' = (input: list<string>, acc: string, vars: list<string>): (
  (list<component>, list<component>),
  list<string>,
) => {
  switch input {
  | list{} => ((list{Protocol(acc)}, strsToVars(vars)), input)
  | list{":", "/", "/", ...rest} => ((list{Protocol(acc)}, strsToVars(vars)), rest)
  | list{"{", "{", ...rest} => {
      let (var, remainder) = Variables.parse(rest)
      parse'(remainder, "", Belt.List.add(vars, var))
    }
  | list{c, ...rest} => parse'(rest, acc ++ c, vars)
  }
}

let parse = (input: list<string>): ((list<component>, list<component>), list<string>) => {
  parse'(input, "", list{})
}

// -- http://user:pwd@server.com:8080/api/path?name=bob&age=13#some-hash

// protocol str <- takeUntil "://" input -> "//:"
// h th tth ptth :ptth /:ptth //:ptth
// -> (http, user:pwd....)

// takeUntil "/" input
// - takeUntil "@"
// - - takeUntil ":"
// - takeUntil ":"

// takeUntil :: string -> input -> (string, string)
