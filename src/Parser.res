type t<'a> = Parser(string => option<(string, 'a)>)
let runParser = (Parser(parser), inp) => parser(inp)
// Functor
let map: (t<'a>, 'a => 'b) => t<'b> = (p, f) => Parser(
  s => runParser(p, s)->Option.map(((s', a)) => (s', f(a))),
)
let subParse: (t<'a>, t<'b>) => t<'b> = (Parser(pa), Parser(pb)) => Parser(
  s => pa(s)->Option.flatMap(((s', a)) => pb(a)->Option.map(((_, b)) => (s', b))),
)
// Alternative
let alt: (t<'a>, t<'a>) => t<'a> = (Parser(p1), Parser(p2)) => Parser(
  s => p1(s)->Option.orElse(p2(s)),
)
let return: (string => option<(string, 'a)>) => t<'a> = f => Parser(f)
// Utils
let collect: t<'a> => t<list<'a>> = (Parser(parser)) => Parser(
  s => {
    let rec run = (s': string, acc: list<'a>): (string, list<'a>) => {
      switch parser(s') {
      | None => (s', acc)
      | Some((s'', a)) => run(s'', list{...acc, a})
      }
    }
    Some(run(s, list{}))
  },
)
let parallel: list<t<'a>> => t<list<'a>> = ps => Parser(
  s => {
    Some(
      ps
      ->List.map(p => runParser(p, s))
      ->List.reduce(("", list{}), ((s', acc), res) => {
        res->Option.mapWithDefault((s', acc), ((s'', a)) => (
          String.length(s') < String.length(s'') ? s' : s'',
          list{...acc, a},
        ))
      }),
    )
  },
)
let tryAll: list<t<'a>> => t<list<'a>> = ps => Parser(
  s => {
    let rec run = (ps: list<t<'a>>, s': string, acc: list<'a>): (string, list<'a>) => {
      switch ps {
      | list{} => (s', acc)
      | list{Parser(p), ...ps'} =>
        switch p(s') {
        | None => run(ps', s', acc)
        | Some((s'', a)) => run(ps', s'', list{...acc, a})
        }
      }
    }
    Some(run(ps, s, list{}))
  },
)

let takeUntil = (em: string): t<string> => {
  return(input =>
    String.indexOfOpt(input, em)->Option.map(ei => (
      String.substringToEnd(input, ~start=ei + String.length(em)),
      String.substring(input, ~start=0, ~end=ei),
    ))
  )
}

let takeFrom = (sm: string): t<string> => {
  return(input =>
    String.indexOfOpt(input, sm)->Option.map(si => (
      "",
      String.substringToEnd(input, ~start=si + String.length(sm)),
    ))
  )
}

let takeBetween = (sm: string, em: string): t<string> => {
  return(input =>
    String.indexOfOpt(input, sm)->Option.flatMap(si => {
      String.indexOfOpt(input, em)->Option.map(
        ei => {
          (
            String.substringToEnd(input, ~start=ei + String.length(em)),
            String.substring(input, ~start=si + String.length(sm), ~end=ei),
          )
        },
      )
    })
  )
}
