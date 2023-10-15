open ComponentParsers
open Component

@genType
let urlParser = Parser.parallel(list{
  allVarsStr,
  list{
    parseProtocol,
    parseUserInfo,
    parseDomain,
    parsePort,
    parsePath,
    parseQueryString,
    parseHash,
  }->Parser.tryAll,
})

// Library interface
@genType
type credentials = {
  userName: string,
  password: string,
}

@genType
type urlRecord = {
  protocol: option<string>,
  userInfo: option<credentials>,
  domain: option<array<string>>,
  port: option<string>,
  path: option<array<string>>,
  query: option<array<array<string>>>,
  hash: option<string>,
  variables: option<array<string>>,
}

let initial: urlRecord = {
  protocol: None,
  userInfo: None,
  domain: None,
  port: None,
  path: None,
  query: None,
  hash: None,
  variables: None,
}

let toResults = (xs: list<component>): urlRecord =>
  xs->List.reduce(initial, (acc, cmp) => {
    switch cmp {
    | Protocol(x) => {...acc, protocol: Some(x)}
    | UserInfo(userName, password) => {...acc, userInfo: Some({userName, password})}
    | Domain(xs) => {...acc, domain: Some(xs)}
    | Port(x) => {...acc, port: Some(x)}
    | Path(xs) => {
        ...acc,
        path: Some(xs),
      }
    | Query(xs) => {...acc, query: xs->Array.map(((k, v)) => [k, v])->Some}
    | Hash(x) => {...acc, hash: Some(x)}
    | Variable(x) => {
        ...acc,
        variables: acc.variables->Option.map(xs => Array.concat(xs, [x]))->Option.orElse(Some([x])),
      }
    }
  })
