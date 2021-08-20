(** 
Module provides functionality to
get content from source file path
after checking its file type.
*)

module InputFileHandler = struct
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
let isAsmFile filename = 
    let l = length filename
    in l >= 4 && sub filename (l - 4) 4 = ".asm"

(**
Returns content of input channel as a list of strings/lines
*)

let readAllLines file = 
    let rec readLines acc f =
    try readLines ((input_line f) :: acc) f
    with End_of_file -> rev acc
    in readLines [] file 


(**
Opens new input channel, raises FileNoTFound exception
*)
let openFile filename =
    try open_in filename with Sys_error _ -> raise (fileNotFoundError filename)


(*
Returns content of file specified by path 'filename'
as a list of strings/lines, 
raises FileNotFound and IllegalFileType exceptions
*)
let getContent (filename : string) =
    if isAsmFile filename then
    readAllLines (openFile filename)
    else raise illegalFileError 

end
