open Sourcecodestructure

module SourceCodeFormatter = struct
(**
Module provides functionality to
format Assembly source code
by means of ommiting empty lines,
front-line and extra whitespaces
*)

open SourceCodeStructure
open List
open String

let formatting_error_message = "compiling error."

(**
Concatenates formatted
line to a single string
*)
let reassemble = concat " "

(**
Extracts mnemonic from line
*)
let get_mnemonic line=
    match line with
        |[] -> failwith formatting_error_message
        |x::_ -> x

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
Removes extra whitespaces
in a string and returns
list of words
*)
let format_line line = (filterWords (get_words line))

(**
Returns true if line does not consist of
any character other than whitespace,
false otherwise
*)
let rec is_blank_line line =
    let l = length line 
    in
    match l with
        |0 -> true
        |x -> (sub line 0 1 = " ") && (is_blank_line (sub line 1 (l-1)))

(**
Formats list of assembly
source code lines and returns
formatted source code lines.
Ommiting blank lines and whitespaces
*)
let format_code (code : sourceCode) : formattedSourceCode =
    let rec format_code_impl acc c =
        match c with
            |[] -> rev acc
            |x :: xs -> let line = get_line x 
            in if is_blank_line line 
                            then format_code_impl acc xs 
                            else format_code_impl (((format_line line), get_number x) :: acc) xs

    in format_code_impl [] code

end