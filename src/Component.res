@genType
type component =
  | Protocol(string)
  | Domain(list<string>)
  | Path(list<string>)
  | Query(list<list<string>>)
  | Hash(string)
  | Variable(string)
