(* Uses Core, where Set.add and Map.add take their arguments in a different
   order, so only Set.union should get an [empty] point. *)
open! Core

let widen (a : Int.Set.t) (b : Int.Set.t) : Int.Set.t = Set.add (Set.union a b) 3
let index (m : int String.Map.t) : int String.Map.t = Map.add_exn m ~key:"k" ~data:1
