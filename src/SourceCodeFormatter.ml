(**
Module provides functionality to
format Assembly source code
by means of ommiting empty lines,
front-line and extra whitespaces
*)
module SourceCodeFormatter = struct
open List
open String

(**
Returns line as a list of words
with no whitespaces
*)
let get_words line = split_on_char ' ' line

(**
Removes empty words
from the list of words
*)
let filterWords = filter (fun x -> x <> "")

(**
Concatenates the list of words
as space separated line
*)
let concat_line = concat " "

(**
Removes extra whitespaces 
in a string
*)
let parseLine line = (filterWords (get_words line))

(**
Returns true if line does not consist of
any character other than whitespace,
false otherwise
*)
let rec isBlankLine line =
    let l = length line 
    in
    match l with
        |0 -> true
        |x -> if sub line 0 1 = " " then isBlankLine (sub line 1 (l-1)) else false

(*
Formats list of assembly
source code lines and returns
formatted source code lines (list of words).
Ommiting blank lines and whitespaces
*)
let parseCode (code : string list) =
    let rec parseCodeImpl acc c =
        match c with
            |[] -> rev acc
            |x :: xs -> if isBlankLine x 
                            then parseCodeImpl acc xs 
                            else parseCodeImpl ((parseLine x) :: acc) xs

    in parseCodeImpl [] code

end