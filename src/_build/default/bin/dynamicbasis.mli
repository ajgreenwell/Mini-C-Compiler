(* file: dynamicbasis.mli
 * author: Bob Muller
 * revised as of 2/2019
 *)
type t = CodeGenerator of (Mips.Operand.t list -> Mips.Codestream.t)
       | Offset of int

val codeGenerators : t list

