module type S = Strategy_intf.S

module Make : functor () -> S
