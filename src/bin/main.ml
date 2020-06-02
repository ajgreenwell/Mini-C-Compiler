(* file: compile.ml
   author: R. Muller
   revised: February 2019

  The Compile module implements a gut-simple compiler for the language
  miniC.  The mc compiler generates code for the MIPS processor (as
  implemented by the MARS Simulator). The compiler consists of several
  majors phases, each of which is implemented in a separate module.

   The following switch can be set via the command-line to run the
   type-checker.
*)
let typeChecking = ref true

(* compilerOptions processes compiler options and returns the
   position in Sys.argv where the source file ought to be. Remember
   that Sys.argv.(0) is the string showing the program invocation.
 *)
let compilerOptions() =
  match Array.length(Sys.argv) with
  | 2 -> 1
  |	3 ->
    (match Sys.argv.(1) with
     | "-nocheck" -> typeChecking := false; 2
     | "-t" -> Debug.debugLexer := true; 2
     | _ ->
       failwith (Lib.fmt "Unknow compiler option %s.\n\n" Sys.argv.(1)))
  | _ -> failwith "Too many compiler options.\n\n"

let parseFile fileName =
  let inch = open_in fileName in
  let lexbuf = Lexing.from_channel inch in
  let ast = (if !Debug.debugLexer then
               let _ = Debug.dumpTokens Lexer.token lexbuf
               in
               Ast.Program([])
             else
               Parser.program Lexer.token lexbuf)
  in
  close_in inch ;
  ast

let compile() =
  let n = compilerOptions () in
  let filename = Sys.argv.(n) in
  let dbgOut = Debug.debugSetup filename in
  (*  let _ = Printf.printf "filename : [%s]\n" filename in *)

  (* Build the base type environment. *)
  let typeEnv = Env.make Staticbasis.operatorTypes in

  (* Parse the text in the input file. *)
  let ast = parseFile filename in
  let _ = Debug.debugInfo(dbgOut, "The input program is:", ast) in

  (* See if the user wants this program typed-checked. *)
  let _ = (if !typeChecking then
             let msg = (try
                          let _ = Static.typeCheck typeEnv ast
                          in
                          "\nThe program is well-typed.\n"
                        with Static.TypeError s ->
                          let _ = print_string s in
                          let _ = Util.writeln(dbgOut, 0, s)
                          in
                          failwith "Compilation failed.\n")
             in
             (if !Debug.debug then Util.writeln(dbgOut, 0, msg) else ())
           else
             (* No type checking, pretend the program was well-typed. *)
             ()) in

  (* Check for the presence of main function *)
  let Program ps = ast in
  let _ =
    match List.exists (fun (Ast.Procedure {id; formals; _}) ->
        Symbol.format id = "main" && formals = []) ps with
    | true -> ()
    | false -> failwith "main() not found"
  in

  (* Perform the naming source-to-source transformation.
  *)
  let named = Name.translate ast in
  let _ = Debug.debugInfo(dbgOut, "After the naming phase:", named) in

  (* Remove nested let-defintions, another source-to-source translation.
  *)
  let lifted = Lift.translate named in
  let _ = Debug.debugInfo(dbgOut, "After the lifting phase:", lifted) in

  (* Remove propagated copies.
  *)
  let copy = Copyprop.translate lifted in
  let _ = Debug.debugInfo(dbgOut, "After the copyprop phase:", copy) in

  (* Insert control forms; a translation to the language of quadruples.
  *)
  let quads = Control.translate copy in
  let _ = Util.writeln(dbgOut, 0, "\nAfter the control phase:") in
  let _ = Quads.dumpInstructionStream quads in

  (* Remove propagated copies from the quad stream.

  HEADS UP! Seems there is a bug Copyprop_quads.translate --> diverges.
  FIX

  let copyquads = Copyprop_quads.rmNoop (Copyprop_quads.translate quads) in
  let _ = Util.writeln(dbgOut, 0, "\nAfter the quads copyprop phase:") in
  let _ = Quads.dumpInstructionStream copyquads in
  *)

  (* Produce a code stream --- a sequence of MIPS instructions.
  *)
  let codestream = Codegen.translate quads in
  let _ = (if !Debug.debug then
             let objfname = Util.makeFileName(filename, "asm") in
             let msg = Lib.fmt "\nEmitting MIPS assembley code to %s\n\n" objfname
             in
             (
               Util.writeln(dbgOut, 0, msg);
               close_out(dbgOut)
             )
           else ()) in

  (* Emit the assembley code stream to the output file.  This code can
     be assembled and run using the MARS simulator.
  *)
  let _ = Mips.Codestream.emit(filename, codestream)
  in
  ()

(* Run the compiler
let () = compile ()
*)

(* The tryDispatch function is here to support
   student work on the compiler. Invoking the compiler
   as in

   > dune exec bin/main.exe -try lift

   e.g., will call the Lift.translate function on a
   fixed ast that would be the output of the Name
   transformation. It will pretty-print the result to
   the fixed file see.dbg.
*)
let tryDispatch () =
  match Sys.argv.(2) with
  | "name" -> Try.name ()
  | "lift" -> Try.lift ()
  | "control" -> Try.control ()
  | "codegen" -> Try.codegen ()
  | _ -> failwith "test: unknown compiler phase."

let () =
  if Sys.argv.(1) = "try" then
    tryDispatch()
  else
    compile()
