@genType
type component =
  | Protocol(string)
  | UserInfo(string)
  | Domain(list<string>)
  | Port(string)
  | Path(list<string>)
  | Query(list<list<string>>)
  | Hash(string)
  | Variable(string)
