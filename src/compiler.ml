(** 
-------------------------------------------------------------------
|                     (c) Otar Kalandadze                         |
|MIPS assembly compiler for Kutaisi International University      |
|computer architecture course, based on simple MIPS processor     |
|specification presented in 'System Architecture' by Wolfgang J.  |
|Paul. Specification materials from the book are presented in the |
|project materials.                                               |
|Program is dedicated to the freshmen to make the testing of      |
|their MIPS processor hardware design testing easier, therefore   |
|reusing and redistributing the program is permitted, although    |
|mentioning original author would be appreciated.                 |
|Some specifications of the compiler and language can be          |
|relatively easily modified my making changes in language data    |
|files. Current specifications and syntax features can be found   |
|in README file.                                                  |
-------------------------------------------------------------------


*)

(**

test change git 

TODO
SourceCodeParser,
legal characters list input from database,
create readme file
*)

module CommonFunctions = struct


(**
Checks whether
an el is in list l
*)
let rec is_in l el =
    match l with
    |[] -> false
    |x::xs -> el = x || is_in xs el


end


module SourceCodeStructure = struct
(**
Module provides specification
and functionality of
of raw source code, 
formated source code and
their components.
*)

open String
open CommonFunctions

(**
Specifying legal characters in
assembly source code
*)
let alpha_lower = "a b c d e f g h i j k l m n o p q r s t u v w x y z "
let alpha_upper = uppercase_ascii alpha_lower
let legal_symbols = "$ 0 1 2 3 4 5 6 7 8 9"
let legal_chars = concat "" [alpha_lower; alpha_upper; legal_symbols]
let legal_characters = split_on_char ' ' legal_chars

(**
Checks whether the character
is legal or not.
*)
let is_legal_character = is_in legal_characters

type lineNumber = int

type sourceCodeLine = string 
type sourceCodeLineInfo = sourceCodeLine * lineNumber
type sourceCode = sourceCodeLineInfo list

type formattedSourceCodeLine = string list
type formattedSourceCodeLineInfo = formattedSourceCodeLine * lineNumber
type formattedSourceCode = formattedSourceCodeLineInfo list

type parsedSourceCode = formattedSourceCodeLine list

(**
Extracts line content from source code line info
*)
let get_line (line, _) = line

(**
Extracts line number from source code line info
*)
let get_number (_, number) = number

end


module AssemblyCodeStructure  = struct

open CommonFunctions
open String

type instruction = 
{mnemonic:string; 
argument_number:int; 
instruction_type:char; 
opcode:string; 
fun_code:string; 
rt:string; 
rs:string}


let mnemonics = ["mnemonic"; "mnemonic1"; "mnemonic2"; "mnemonic3"]

(**
Checks whether the word
is a legal mnemonic
*)
let is_legal_mnemonic word = is_in mnemonics (lowercase_ascii word)

end


module Compiler = struct


end





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
        |x -> if sub line 0 1 = " " then is_blank_line (sub line 1 (l-1)) else false

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


module InputFileHandler = struct
(** 
Module provides functionality to
get content from source file path
after checking its file type.
*)

open SourceCodeStructure
open List
open String

(**
Exception raised when the provided file doesn't have extension '.asm'
*)
exception IllegalFileType of string

let illegalFileErrorMessage = "Illegal file type. Please provide file with '.asm' extension"
let illegalFileError = IllegalFileType illegalFileErrorMessage


(**
Exception raised when the provided '.asm' file was not found in current directory
*)
exception FileNotFound of string

let fileNotFoundErrorMessage filename = concat  "" ["File with name '"; filename; "' not found."]
let fileNotFoundError filename = FileNotFound (fileNotFoundErrorMessage filename)


(**
Parses filename (string) for .asm extension
*)
let is_asm_file filename = 
    let l = length filename
    in l >= 4 && sub filename (l - 4) 4 = ".asm"

(**
Returns content of input channel as an
unformatted sourceCode
*)
let readAllLines file : sourceCode = 
    let rec readLines acc lineNum f =
    try readLines ((input_line f, lineNum) :: acc) (lineNum + 1) f
    with End_of_file -> rev acc
    in readLines [] 1 file 


(**
Opens new input channel, raises FileNoTFound exception
*)
let open_file filename =
    try open_in filename with Sys_error _ -> raise (fileNotFoundError filename)


(*
Returns unformatted source code from file
specified by filename, 
raises FileNotFound and IllegalFileType exceptions
*)
let get_content (filename : string) : sourceCode =
    if is_asm_file filename then
    readAllLines (open_file filename)
    else raise illegalFileError 

end

let input = InputFileHandler.get_content

let format filename = SourceCodeFormatter.format_code (input filename)

let parse_characters filename = SourceCodeParser.parse_illegal_chars (format filename)

let parse_mnemonics filename = SourceCodeParser.parse_illegal_mnemonics (format filename)