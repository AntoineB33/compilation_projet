type opComp = Eq | Neq | Lt | Le | Gt | Ge

type expType =
  Id of string
| Cste of int
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| Comp of opComp*expType*expType

type progType = unit (* en attendant mieux ! *)

exception Decl_error of string
exception VC_error of string
exception Internal_error of string
