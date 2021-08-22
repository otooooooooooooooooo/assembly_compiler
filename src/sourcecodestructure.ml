open Commonfunctions

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
