open Types

val assert_type_expression_eq : ( type_expression * type_expression ) -> unit option
val assert_value_eq : ( expression * expression ) -> unit option
val assert_eq : 'a -> 'a -> unit option
