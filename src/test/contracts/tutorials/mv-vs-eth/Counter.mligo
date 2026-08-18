// MAVRYK: restored as a real file (was a symlink into gitlab-pages/website/versioned_docs/
// version-1.6.0/...); that 1.6.0 doc snapshot was retired, breaking the symlink and the
// contract_test `glob_files_rec contracts/*`. Content is the original mv-vs-eth tutorial contract.
type parameter = Add of int | Subtract of int

type storage = int

let main (p, s : parameter * storage) =
  match p with
    Add n -> ([] : operation list), s + n
  | Subtract n -> ([] : operation list), s - n
