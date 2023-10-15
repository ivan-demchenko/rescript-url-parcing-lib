@genType
type component =
  | Protocol(string)
  | UserInfo(string, string)
  | Domain(array<string>)
  | Port(string)
  | Path(array<string>)
  | Query(array<(string, string)>)
  | Hash(string)
  | Variable(string)
