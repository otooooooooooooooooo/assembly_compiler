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

open Inputfilehandler
open Sourcecodeformatter
open Sourcecodeparser

let input = InputFileHandler.get_content

let format filename = SourceCodeFormatter.format_code (input filename)

let parse_characters filename = SourceCodeParser.parse_illegal_chars (format filename)

let parse_mnemonics filename = SourceCodeParser.parse_illegal_mnemonics (format filename)