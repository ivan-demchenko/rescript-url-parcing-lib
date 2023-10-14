open ComponentParsers
open Component

@genType
let parser = Parser.parallel(list{
  allVarsStr,
  list{parseProtocol, parseUserInfo, parseDomain, parsePort, parsePath, queryString}->Parser.tryAll,
  parseHash,
})

// Library interface

@genType
type urlRecord = {
  protocol: option<string>,
  userInfo: option<string>,
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
    | UserInfo(x) => {...acc, userInfo: Some(x)}
    | Domain(xs) => {...acc, domain: Some(xs->List.toArray)}
    | Port(x) => {...acc, port: Some(x)}
    | Path(xs) => {
        ...acc,
        path: Some(xs->List.toArray),
      }
    | Query(xs) => {...acc, query: Some(xs->List.map(p => p->List.toArray)->List.toArray)}
    | Hash(x) => {...acc, hash: Some(x)}
    | Variable(x) => {
        ...acc,
        variables: acc.variables->Option.map(xs => Array.concat(xs, [x]))->Option.orElse(Some([x])),
      }
    }
  })

// Run some tests

let inputs = list{
  "localhost",
  "localhost:8080",
  "http://localhost:8080",
  "http://localhost:8080#test-hash",
  "http://test.com/api/v1",
  "http://test.com/api/v1/",
  "http://test.com/api/v1?a=1",
  "http://test.com/api/v1/?a=1",
  "http://test.com/api/{{api-version}}?a=1&b=2#my-hash",
}

let d1 = Date.now()

inputs->List.forEach(input => {
  Js.Console.log(`Parsing: ${input}`)
  Js.Console.log(
    parser
    ->Parser.runParser(input)
    ->Option.map(((_, xs)) => {
      xs->List.flatten->toResults
    })
    ->Option.getWithDefault(initial),
  )
})

let parsingTime = Float.toString(Date.now() -. d1)

Js.Console.log(`Parsing everything took ${parsingTime}ms`)
