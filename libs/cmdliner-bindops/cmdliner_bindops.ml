module C = Cmdliner

let return = C.Term.const
let map x f = C.Term.app (return f) x
let both a b = C.Term.app (C.Term.app (return (fun a b -> (a, b))) a) b
let (let+) = map
and (and+) = both
