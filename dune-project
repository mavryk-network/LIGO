(lang dune 2.9)
(name ligo)
(using menhir 2.0)
(using coq 0.2)
(using dune_site 0.1)

(package 
 (name ligo)
 (sites (lib ligo_runtime) (lib ligo_wasi))
)