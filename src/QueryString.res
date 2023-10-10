open Components

type parserMode = Key | Value

let rec parse' = (
  mode: parserMode,
  input: list<string>,
  k: string,
  v: string,
  vars: list<string>,
  qps: list<queryParam>,
): ((list<component>, list<component>), list<string>) => {
  switch (mode, input) {
  | (Key, list{"{", "{", ...rest}) => {
      let (var, rest) = Variables.parse(rest)
      parse'(Key, rest, `${k}{{${var}}}`, v, Belt.List.add(vars, var), qps)
    }
  | (Key, list{}) => ((list{Query(qps)}, strsToVars(vars)), list{})
  | (Key, list{c, ...rest}) =>
    switch c {
    | "=" => parse'(Value, rest, k, "", vars, qps)
    | c => parse'(Key, rest, k ++ c, "", vars, qps)
    }
  | (Value, list{"{", "{", ...rest}) => {
      let (var, rest) = Variables.parse(rest)
      parse'(Key, rest, k, `${v}{{${var}}}`, Belt.List.add(vars, var), qps)
    }
  | (Value, list{}) => {
      let query = list{Query(list{...qps, (k, v)})}
      ((query, strsToVars(vars)), list{})
    }
  | (Value, list{c, ...rest}) =>
    switch c {
    | "&" => parse'(Key, rest, "", "", vars, list{...qps, (k, v)})
    | c => parse'(Value, rest, k, v ++ c, vars, qps)
    }
  }
}

let parse = (input: list<string>): ((list<component>, list<component>), list<string>) => {
  parse'(Key, input, "", "", list{}, list{})
}
