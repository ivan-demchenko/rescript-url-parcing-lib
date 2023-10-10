let joinList = (xs: list<string>, del: string): string => {
  xs->Belt.List.toArray->Js.Array2.joinWith(del)
}

type queryParam = (string, string)

let pairToArr = ((k, v): queryParam) => [k, v]

type component =
  | Protocol(string)
  | Domain(string)
  | Path(list<string>)
  | Query(list<queryParam>)
  | Variable(string)

let queryParamsToStr = (xs: list<queryParam>): string => {
  xs->Belt.List.toArray->Js.Array2.map(((k, v)) => `${k}=${v}`)->Js.Array2.joinWith(", ")
}

let strsToVars = vars => Belt.List.map(vars, x => Variable(x))

let componentToStr = comp =>
  switch comp {
  | Domain(str) => `Domain(${str})`
  | Protocol(str) => `Protocol(${str})`
  | Path(paths) => `Path(${paths->Belt.List.reverse->joinList(", ")})`
  | Query(kvs) => `Query(${queryParamsToStr(kvs)})`
  | Variable(var) => `Var(${var})`
  }
