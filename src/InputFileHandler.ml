open Sourcecodestructure


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
and closes file
*)
let readAllLines file : sourceCode = 
    let rec readLines acc lineNum f =
    try readLines ((input_line f, lineNum) :: acc) (lineNum + 1) f
    with End_of_file -> close_in file; rev acc
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
