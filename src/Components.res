type component =
  | Protocol(string)
  | Domain(list<string>)
  | Path(string)
  | Query(list<string>)
  | Hash(string)
  | Variable(string)
