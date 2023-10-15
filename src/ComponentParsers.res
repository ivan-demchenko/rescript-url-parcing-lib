open Parser
open Component

let nonEmptyStr = s => String.length(s) > 0

@genType
let allVarsStr =
  takeBetween("{{", "}}")->map(x => x->Variable)->collect

@genType
let parseProtocol = takeUntil("://")->map(x => x->Protocol)

@genType
let parseUserInfo =
  takeFrom("://")
  ->alt(takeAll)
  ->subParse(takeUntil("@"))
  ->map(s => {
    switch String.split(s, ":") {
    | [user, pwd] => UserInfo(user, pwd)
    | _ => UserInfo(s, "")
    }
  })

@genType
let parseDomain =
  takeFrom("://")
  ->alt(takeFrom("@"))
  ->alt(takeAll)
  ->subParse(takeUntil(":")->alt(takeUntil("/"))->alt(takeUntil("#"))->alt(takeAll))
  ->map(str => str->String.split(".")->Domain)

let parsePort =
  takeFrom(":")->subParse(takeUntil("/")->alt(takeUntil("#"))->alt(takeAll))->map(x => x->Port)

@genType
let parsePath =
  takeFrom("/")
  ->subParse(takeUntil("?")->alt(takeUntil("#"))->alt(takeAll))
  ->map(str => str->String.split("/")->Array.filter(nonEmptyStr)->Path)

@genType
let parseHash = takeFrom("#")->Parser.map(x => Hash(x))

@genType
let parseQueryString = takeFrom("?")->subParse(
  takeUntil("#")
  ->alt(takeAll)
  ->Parser.map(str =>
    str
    ->String.split("&")
    ->Array.filter(nonEmptyStr)
    ->Array.reduce([], (acc, strPair) => {
      switch strPair->String.split("=") {
      | [k, v] => Array.concat(acc, [(k, v)])
      | _ => acc
      }
    })
    ->Query
  ),
)
