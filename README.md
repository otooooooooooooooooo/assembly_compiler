# assembly_compiler
Assembly to machine code compiler for MIPS processor

#TODO syntax features, specifications, user manual



Compilation and run:

1. DOWNLOAD src folder

2. in terminal window, MOVE TO src folder

3. (if you made changes in ml files) to RECOMPILE, enter:

ocamlc .\commonfunctions.ml .\sourcecodestructure.ml .\inputfilehandler.ml .\sourcecodeformatter.ml .\assemblycodestructure.ml .\sourcecodeparser.ml 

4. to RUN: 
open utop and enter: 

#load "commonfunctions.cmo";;
#load "sourcecodestructure.cmo";;
#load "inputfilehandler.cmo";;
#load "sourcecodeformatter.cmo";;
#load "assemblycodestructure.cmo";;
#load "sourcecodeparser.cmo";;
#use "compiler.ml";;
