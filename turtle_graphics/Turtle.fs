module Turtle

open Mini
open Types


let right (angle: Double): Command =
    L -angle

let rec substF (transformF: Double -> Program) (p: Program): Program =
    match p with
    | []            ->  []
    | (F len)::xs   ->  transformF len @ substF transformF xs
    | cmd::xs       ->  cmd :: (substF transformF xs)  


// Levy
let levyStart (len: Double) =
    [D; F len]

let levyTransform (len: Double): Program =
    let sqrtLen = len / sqrt(2.0)
    [L 45.0; F sqrtLen; right 90.0; F sqrtLen; L 45.0]


// kochFlocke
// F
let kochflockeStart (len: Double) =
    let a = 60.0
    [D; F len; right (2.0*a); F len; right (2.0*a); F len]

// +F--F+
let kochflockeTransform (len: Double): Program =
    let l = len / 3.0
    let a = 60.0
    let flf = [F l; L a; F l]
    flf @ [right (2.0*a)] @ flf


// pentaPlexity
// F++F++F++F++F
let pentaplexityStart (len: Double) =
    let a = 36.0
    let lf = [L (2.0*a); F len]
    [D; F len] @ lf @ lf @ lf @ lf

// F -> F++F++F|F-F++F
let pentaplexityTransform (len: Double): Program =
    let phi = (1.0 + sqrt 5.0) / 2.0
    let l = len / (phi ** 2.0)
    let a = 36.0
    [F l; L (2.0*a); F l; L (2.0*a); F l; L 180.0; F l; right a; F l; L (2.0*a); F l]