#import "gitlab-pages/docs/modules/src/euro.ligo" "Euro"

type storage is Euro.t

function tip (const s : storage) : storage is Euro.add (s, Euro.one)