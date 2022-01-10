module Nim
open Mini
open NimTypes


//queries natural number
let rec queryNat (msg: String): Nat =
    putstring msg
    let str = getline ()

    if str <> "" && String.forall Char.IsDigit str then
        putline ("The number " + string str + " was entered.")
        readNat str
    else
        putline "Input is not a natural number!"
        queryNat msg


//queries valid move
let rec queryMove (n: Nat) (k: Nat) (p: Player): Nat =
    let num = queryNat ("There are " + string n + " matches left. It's player " + string p + "'s turn: ")

    if (num < 1N || num > k) then
        putline ("Invalid input!")
        queryMove n k p      
    else
        num


//evaluates moves with winning condition
let rec nim (n: Nat) (k: Nat) (p: Player): Unit = 
    if n > 0N then
        nim (n - queryMove n k p) k (if p = A then B else A)
    else
        putline ("Player " + string p + " wins the game!")


let main(): Unit =
    putline "Welcome to Nim"
    putstring "How many matches to play with? "
    let n = getline ()
    putline ("The number " + n + " was entered.")
    nim (readNat n) 3N A
