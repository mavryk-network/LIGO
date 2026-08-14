type committee is record [ members : list (address); quorum : nat ]

type leader is record [ name : string; address : address ]

type authority is
  Dictatorship of leader
| Democracy of committee