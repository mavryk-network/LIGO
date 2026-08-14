#import "gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main.ligo" "C"

module Proxy is {

  [@entry]
  function proxy (const p : C.parameter; const s : C.storage) : list (operation) * C.storage is
    C.main (p, s)

}