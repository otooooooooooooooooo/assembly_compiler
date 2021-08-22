open Sourcecodestructure
open Sourcecodeformatter
open Assemblycodestructure

module SourceCodeParser = struct
(**
Module providing parser
for source code.
Parser checks for
illegal character presence,
illegal operation or statement presence
and builds compile-ready objects. 
*)

open SourceCodeStructure
open SourceCodeFormatter
open AssemblyCodeStructure
open List
open String

exception IllegalCharacter of string

let illegalCharacterErrorMessage (line, linenumber) = 
    concat "" ["Illegal character at line "; string_of_int linenumber; ": '"; reassemble line; "'."]

let illegalCharacterError lineInfo = IllegalCharacter (illegalCharacterErrorMessage lineInfo)

exception IllegalMnemonic of string

let illegalMnemonicErrorMessage (line, linenumber) =
    concat "" ["Illegal mnemonic at line "; string_of_int linenumber; ": '"; get_mnemonic line; "'."]

let illegalMnemonicError lineInfo = IllegalMnemonic (illegalMnemonicErrorMessage lineInfo)

(**
Checks whether the word
consists of only legal
characters
*)
let rec is_legal_word word =
    let l = length word
    in 
    if l = 0 then true
    else (is_legal_character (sub word 0 1)) && (is_legal_word (sub word 1 (l - 1)))

(**
Checks whether the line
consists of only legal
characters
*)
let rec is_legal_line line =
    match line with
        |[] -> true
        |x::xs -> (is_legal_word x) && (is_legal_line xs)

(**
Extracts line and 
parses for legal characters
*)
let is_legal_source_line sourceLine = is_legal_line (get_line sourceLine)

(**
Takes code and parses each line
with 'parser' function, raises
'exn' exception
*)
let rec parse_concrete parser exn (code : formattedSourceCode) =
    match code with
        |[] -> ()
        |x::xs -> if not(parser x) 
                    then raise (exn x)
                    else parse_concrete parser exn xs

(**
Parses the formatted source code
for illegal characters. Raises
IllegalCharacter exception.
*)
let rec parse_illegal_chars = parse_concrete is_legal_source_line illegalCharacterError


(**
Extracts line from
source and checks for
legal mnemonic
*)
let has_legal_mnemonic (sourceLine : formattedSourceCodeLineInfo) = 
    is_legal_mnemonic (get_mnemonic (get_line sourceLine))

(**
Parses code for illegal
mnemonics, raises illegalMnemonic
exception
*)
let parse_illegal_mnemonics = parse_concrete has_legal_mnemonic illegalMnemonicError

(**
TODO
parse illegal statement structures
translate to instruction objects
*)

end
