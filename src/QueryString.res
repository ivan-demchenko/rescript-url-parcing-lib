open Components

type parserMode = Key | Value

let rec _parse = (mode: parserMode, input: list<string>, k: string, v: string, res: queryParams): (
  option<component>,
  list<string>,
) => {
  switch (mode, input) {
  | (Key, list{}) => (Some(Query(res)), list{})
  | (Key, list{c, ...rest}) =>
    switch c {
    | "=" => _parse(Value, rest, k, "", res)
    | c => _parse(Key, rest, k ++ c, "", res)
    }
  | (Value, list{}) => (Some(Query(list{...res, (k, v)})), list{})
  | (Value, list{c, ...rest}) =>
    switch c {
    | "&" => _parse(Key, rest, "", "", list{...res, (k, v)})
    | c => _parse(Value, rest, k, v ++ c, res)
    }
  }
}

let parse = (input: list<string>): (option<component>, list<string>) => {
  _parse(Key, input, "", "", list{})
}
