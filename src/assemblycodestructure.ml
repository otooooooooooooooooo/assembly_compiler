open Commonfunctions

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


let mnemonics = ["mnemonic"; "mnemoni"; "mnemonic22"; "mnemonicccc3"]

(**
Checks whether the word
is a legal mnemonic
*)
let is_legal_mnemonic word = is_in mnemonics (lowercase_ascii word)

end
