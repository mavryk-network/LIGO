#import "gitlab-pages/docs/language-basics/src/modules/imported.ligo" "EURO"

type storage is EURO.t

[@entry] function main (const _action : unit; const store : storage) : list (operation) * storage is
  ((nil : list (operation)), EURO.add (store, EURO.one))