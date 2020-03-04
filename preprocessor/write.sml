(*
	IOType
	A serialization preprocessor for Standard ML
	Copyright 2010, 2011 Christopher Cramer

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
functor Write (structure Ast: AST) :> sig
	val writeProgram: string * string * TextIO.outstream * Ast.Program.t -> unit
end = struct

exception NotTransformed

structure Output :> sig
	type t
	val fromOutstream: {inputName: string, outputName: string, outstream: TextIO.outstream} -> t
	val inputPosition: t * Region.t -> unit
	val outputPosition: t -> unit
	val text: t * string -> unit
end = struct
	datatype emitted = InputPosition | InputText | OutputPosition | OutputText
	datatype t = T of {
		inputName: string
		, outputName: string
		, outstream: TextIO.outstream
		, emitted: emitted ref
		, line: int ref
		, column: int ref
	}
	fun fromOutstream {inputName, outputName, outstream} = T {
		inputName = inputName
		, outputName = outputName
		, outstream = outstream
		, emitted = ref OutputPosition
		, line = ref 1
		, column = ref 1
	}
	fun assemble (file, line, column) = String.concat [
		"\n(*#line "
		, Int.toString line
		, "."
		, Int.toString column
		, " \""
		, file
		, "\"*)"
	]
	fun calculate (T {line, column, ...}, string) =
		let
			fun loop (line, column, index) =
				if index = size string then {line = line, column = column}
				else case String.sub (string, index) of
					#"\n" => loop (line + 1, 1, index + 1)
					| _ => loop (line, column + 1, index + 1)
		in
			loop (!line, !column, 0)
		end
	fun text (t as T {outstream, line, column, emitted, ...}, string) =
		case string of
			"" => ()
			| _ =>
				let
					val {line = newLine, column = newColumn} =
						calculate (t, string)
				in
					line := newLine
					; column := newColumn
					; TextIO.output (outstream, string)
					; case !emitted of
						InputPosition => emitted := InputText
						| OutputPosition => emitted := OutputText
						| _ => ()
				end
	fun outputPosition (t as T {outputName, outstream, emitted, line, column, ...}) =
		let
			fun loop (proposedLine, proposedColumn) =
				let
					val string = assemble (
						outputName
						, proposedLine
						, proposedColumn
					)
					val {line = newLine, column = newColumn} =
						calculate (t, string)
				in
					if
						proposedLine = newLine
						andalso proposedColumn = newColumn
					then (
						emitted := OutputPosition
						; text (t, string)
					) else loop (newLine, newColumn)
				end
			fun go () = loop (!line, !column)
		in
		case !emitted of
			OutputPosition => ()
			| OutputText => ()
			| InputPosition => go ()
			| InputText => go ()
		end
	fun inputPosition (t as T {inputName, outstream, line, column, emitted, ...}, region) =
		let
			fun go () = case Region.left region of
				NONE => ()
				| SOME position => 
					let
						val string = assemble (
							inputName
							, SourcePos.line position
							, SourcePos.column position
						)
					in
						emitted := InputPosition
						; text (t, string)
					end
		in
			case !emitted of
				InputPosition => ()
				| _ => go ()
		end
end

structure Vector: sig
	include VECTOR
	val appSeparate: ('a -> unit) * (unit -> unit) -> 'a vector -> unit
end = struct
	open Vector
	fun appSeparate (f, g) v = case Vector.length v of
		0 => ()
		| n => (
			f (Vector.sub (v, 0))
			; let fun loop i = (
				if i < n then (
					g ()
					; f (Vector.sub (v, i))
					; loop (i + 1)
				) else ()
			) in loop 1 end
		)
end
structure List: sig
	include LIST
	val appSeparate: ('a -> unit) * (unit -> unit) -> 'a list -> unit
end = struct
	open List
	fun appSeparate (f, g) l =
		let
			fun loop l = case l of
				nil => ()
				| x :: nil => f x
				| x :: y => (f x; g (); loop y)
		in
			loop l
		end
end

fun writeRegion (p, r) = case Region.left r of
	NONE => Output.outputPosition p
	| SOME _ => Output.inputPosition (p, r)
fun curry f a b = f (a, b)
fun curryOutput (p, s) () = Output.text (p, s)
fun writeSigid (p, x) = (
	writeRegion (p, Ast.Sigid.region x)
	; Output.text (p, Ast.Sigid.toString x)
	; Output.text (p, " ")
)
fun writeTyvar (p, x) = (
	writeRegion (p, Ast.Tyvar.region x)
	; Output.text (p, Ast.Tyvar.toString x)
	; Output.text (p, " ")
)
fun writeTyvars (p, v) =
	case Vector.length v of
		0 => ()
		| 1 => (
			writeTyvar (p, Vector.sub (v, 0))
			; Output.text (p, " ")
		) | _ => (
			Output.text (p, "(")
			; Vector.appSeparate
				(fn x => writeTyvar (p, x), curryOutput (p, ", "))
				v
			; Output.text (p, ") ")
		)
fun writeLongtyconId (p, x) = (
	(* writeRegion (p, Ast.Longtycon.Id.region x)
	; *)Output.text (p, Ast.Longtycon.Id.toString x)
)
fun writeStrid (p, x) = (
	(* writeRegion (p, Ast.Strid.region x)
	; *)Output.text (p, Ast.Strid.toString x)
)
fun writeStrids (p, x) = app (fn y => (writeStrid (p, y); Output.text (p, "."))) x
fun writeLongtycon (p, x) =
	let
		val Ast.Longtycon.T { strids, id } = Ast.Longtycon.node x
	in
		writeStrids (p, strids)
		; writeLongtyconId (p, id)
		; Output.text (p, " ")
	end
fun writeTycon (p, x) = (
	writeRegion (p, Ast.Tycon.region x)
	; Output.text (p, Ast.Tycon.toString x)
	; Output.text (p, " ")
)
fun writeOfType (p, t) = case t of
	NONE => ()
	| SOME x => (
		Output.text (p, "of ")
		; writeType (p, x)
	)
and writeCon (p, c) = (
	writeRegion (p, Ast.Con.region c)
	; Output.text (p, Ast.Con.toString c)
	; Output.text (p, " ")
) and writeCons s (p, v) =
	Vector.appSeparate (
		fn (c, t) => (
			writeCon (p, c)
			; writeOfType (p, t)
		), curryOutput (p, s)
	) v
and writeDatBindCons x = writeCons "| " x
and writeExceptionCons x = writeCons "and " x
and writeTypeLeft (p, { tycon, tyvars }) = (
	writeTyvars (p, tyvars)
	; writeTycon (p, tycon)
) and writeDatBind (p, x) =
	let
		val (
			Ast.DatBind.T { datatypes, withtypes }
			, region
		) = Ast.DatBind.dest x
	in
		writeRegion (p, region)
		; Vector.appSeparate (
			fn { cons, tycon, tyvars } => (
				writeTypeLeft (p, { tycon = tycon, tyvars = tyvars })
				; Output.text (p, " = ")
				; writeDatBindCons (p, cons)
			), curryOutput (p, "and ")
		) datatypes
	end
and writeRepl (p, { lhs, rhs }) = (
	writeTycon (p, lhs)
	; Output.text (p, " = datatype ")
	; writeLongtycon (p, rhs)
) and writeDatatypeRhs (p, x) =
	let
		val (node, region) = Ast.DatatypeRhs.dest x
	in
		writeRegion (p, region)
		; Output.text (p, "datatype ")
		; case node of
			Ast.DatatypeRhs.DatBind y => writeDatBind (p, y)
			| Ast.DatatypeRhs.Repl y => writeRepl (p, y)
	end
and writeTypedescs (p, v) = (
	Output.text (p, "type ")
	; Vector.appSeparate (
		fn left => writeTypeLeft (p, left)
		, curryOutput (p, "and ")
	) v
) and writeTypBind (p, w) =
	let
		val (Ast.TypBind.T v, region) = Ast.TypBind.dest w
	in
		Output.text (p, "type ")
		; writeRegion (p, region)
		; Vector.appSeparate (
			fn { def, tycon, tyvars } => (
				writeTypeLeft (p, { tycon = tycon, tyvars = tyvars })
				; Output.text (p, " = ")
				; writeType (p, def)
			), curryOutput (p, "and ")
		) v
	end
and writeSpecException (p, x) = (
	Output.text (p, "exception ")
	; writeExceptionCons (p, x)
) and writeWhere (p, { longtycon, ty, tyvars }) = (
	Output.text (p, "type ")
	; writeTyvars (p, tyvars)
	; writeLongtycon (p, longtycon)
	; Output.text (p, " = ")
	; writeType (p, ty)
)
and writeWheres (p, (t, v)) = (
	writeSigexp (p, t)
	; Output.text (p, "where ")
	; Vector.appSeparate (
		fn x => writeWhere (p, x)
		, curryOutput (p, "and ")
	) v
)
and writeTypeConstruction (p, (constructor, arguments)) = (
	writeTypeArguments (p, arguments)
	; writeLongtycon (p, constructor)
	; Output.text (p, " ")
)
and writeTypeArguments (p, v) =
	case Vector.length v of
		0 => ()
		| 1 => (
			writeType (p, Vector.sub (v, 0))
			; Output.text (p, " ")
		) | _ => (
			Output.text (p, "(")
			; Vector.appSeparate (
				fn y => writeType (p, y)
				, curryOutput (p, ", ")
			) v
			; Output.text (p, ") ")
		)
and writeTuple (p, v) =
	if Vector.length v = 0 then Output.text (p, "unit ")
	else (
		Output.text (p, "(")
		; Vector.appSeparate (
			fn t => writeType (p, t)
			, curryOutput (p, " * ")
		) v
		; Output.text (p, ")")
	)
and writeRecord (p, r) =
	case Ast.SortedRecord.detupleOpt r of
		NONE => writeActualRecord (p, r)
		| SOME t => writeTuple (p, t)
and writeActualRecord (p, r) = (
	Output.text (p, "{")
	; Vector.appSeparate (
		fn (f, t) => (
			Output.text (p, Ast.SortedRecord.Field.toString f)
			; Output.text (p, " : ")
			; writeType (p, t)
		), curryOutput (p, ", ")
	) (Ast.SortedRecord.toVector r)
	; Output.text (p, "}")
) and writeTypeArrow (p, (a, b)) = (
	writeType (p, a)
	; Output.text (p, " -> ")
	; writeType (p, b)
) and writeTypeGroup (p, g) = (
	Output.text (p, "(")
	; writeType (p, g)
	; Output.text (p, ")")
) and writeType (p, x) =
	let
		val (node, region) = Ast.Type.dest x
	in
		writeRegion (p, region)
		; case node of
			Ast.Type.Arrow a => writeTypeArrow (p, a)
			| Ast.Type.Con c => writeTypeConstruction (p, c)
			| Ast.Type.Group g => writeTypeGroup (p, g)
			| Ast.Type.Record r => writeRecord (p, r)
			| Ast.Type.Var v => writeTyvar (p, v)
	end
and writeSpecSeq (p, (a, b)) = (
	writeSpec (p, a)
	; writeSpec (p, b)
)
and writeSpec (p, w) =
	let
		val (node, region) = Ast.Spec.dest w
	in
		writeRegion (p, region)
		; case node of
			Ast.Spec.Datatype y => writeDatatypeRhs (p, y)
			| Ast.Spec.Empty => ()
			| Ast.Spec.Eqtype y => writeTypedescs (p, y)
			| Ast.Spec.Exception y => writeSpecException (p, y)
			| Ast.Spec.IoDatatype _ => raise NotTransformed
			| Ast.Spec.IoEqtype _ => raise NotTransformed
			| Ast.Spec.IoType _ => raise NotTransformed
			| Ast.Spec.IoTypeDefs _ => raise NotTransformed
			| Ast.Spec.IncludeSigexp y => writeIncludeSigexp (p, y)
			| Ast.Spec.IncludeSigids y => writeIncludeSigids (p, y)
			| Ast.Spec.Seq y => writeSpecSeq (p, y)
			| Ast.Spec.Sharing y => writeSharing (p, y)
			| Ast.Spec.Structure y => writeSpecStructures (p, y)
			| Ast.Spec.Type y => writeTypedescs (p, y)
			| Ast.Spec.TypeDefs y => writeTypBind (p, y)
			| Ast.Spec.Val y => writeVal (p, y)
	end
and writeSig (p, w) = (
	Output.text (p, "sig ")
	; writeSpec (p, w)
	; Output.text (p, "end ")
)
and writeSigexp (p, w) =
	let
		val (node, region) = Ast.Sigexp.dest w
	in
		writeRegion (p, region)
		; case node of
			Ast.Sigexp.Var y => writeSigid (p, y)
			| Ast.Sigexp.Where y => writeWheres (p, y)
			| Ast.Sigexp.Spec y => writeSig (p, y)
	end
and writeIncludeSigexp (p, x) = (
	Output.text (p, "include ")
	; writeSigexp (p, x)
)
and writeIncludeSigids (p, v) = (
	Output.text (p, "include ")
	; Vector.app (fn y => writeSigid (p, y)) v
)
and writeLongstridId (p, x) = (
	(* writeRegion (p, Ast.Longstrid.Id.region x)
	; *)Output.text (p, Ast.Longstrid.Id.toString x)
)
and writeLongstrid (p, w) =
	let
		val Ast.Longstrid.T { strids, id } = Ast.Longstrid.node w
	in
		writeStrids (p, strids)
		; writeLongstridId (p, id)
		; Output.text (p, " ")
	end
and writeEquationStructure (p, l) =
	List.appSeparate (
		fn x => writeLongstrid (p, x)
		, curryOutput (p, " = ")
	) l
and writeEquationType (p, l) = (
	Output.text (p, "type ")
	; List.appSeparate (
		fn x => writeLongtycon (p, x)
		, curryOutput (p, " = ")
	) l
)
and writeEquation (p, w) = (
	let
		val (node, region) = Ast.Equation.dest w
	in
		writeRegion (p, region)
		; Output.text (p, "sharing ")
		; case node of
			Ast.Equation.Structure l => writeEquationStructure (p, l)
			| Ast.Equation.Type l => writeEquationType (p, l)
	end
)
and writeSharing (p, { equations, spec }) = (
	writeSpec (p, spec)
	; Vector.app (fn e => writeEquation (p, e)) equations
)
and writeSpecStructure (p, (strid, sigexp)) = (
	writeStrid (p, strid)
	; Output.text (p, ": ")
	; writeSigexp (p, sigexp)
)
and writeSpecStructures (p, v) = (
	Output.text (p, "structure ")
	; Vector.appSeparate (
		fn x => writeSpecStructure (p, x)
		, curryOutput (p, "and ")
	) v
)
and writeVar (p, x) = (
	writeRegion (p, Ast.Var.region x)
	; Output.text (p, Ast.Var.toString x)
	; Output.text (p, " ")
)
and writeVal (p, v) = (
	Output.text (p, "val ")
	; Vector.appSeparate (
		fn (v, t) => (
			writeVar (p, v)
			; Output.text (p, ": ")
			; writeType (p, t)
		), curryOutput (p, "and ")
	) v
)
fun writeSignature (p, v) = (
	Output.text (p, "signature ")
	; Vector.appSeparate (
		fn (id, exp) => (
			writeSigid (p, id)
			; Output.text (p, " = ")
			; writeSigexp (p, exp)
		), fn () => Output.text (p, "and ")
	) v
)
fun writeStrdecLocal (p, (a, b)) = (
	Output.text (p, "local ")
	; writeStrdec (p, a)
	; Output.text (p, "in ")
	; writeStrdec (p, b)
	; Output.text (p, "end ")
) and writeDecLocal (p, (a, b)) = (
	Output.text (p, "local ")
	; writeDec (p, a)
	; Output.text (p, "in ")
	; writeDec (p, b)
	; Output.text (p, "end ")
) and writeLongconId (p, x) = (
	(* writeRegion (p, Ast.Longcon.Id.region x)
	; *)Output.text (p, Ast.Longcon.Id.toString x)
	; Output.text (p, " ")
) and writeLongcon (p, x) =
	let
		val Ast.Longcon.T { strids, id } = Ast.Longcon.node x
	in
		writeStrids (p, strids)
		; writeLongconId (p, id)
	end
and writeEbRhs (p, x) = (
	writeRegion (p, Ast.EbRhs.region x)
	; case Ast.EbRhs.node x of
		Ast.EbRhs.Def y => (
			Output.text (p, " = ")
			; writeLongcon (p, y)
		) | Ast.EbRhs.Gen (SOME y) => (
			Output.text (p, "of ")
			; writeType (p, y)
		) | Ast.EbRhs.Gen NONE => ()
) and writeDecException (p, (c, e)) = (
	writeCon (p, c)
	; writeEbRhs (p, e)
) and writeDecExceptions (p, v) = (
	Output.text (p, "exception ")
	; Vector.appSeparate (
		fn x => writeDecException (p, x)
		, curryOutput (p, "and ")
	) v
) and writeFixity (p, x) =
	case (
		case x of
			Ast.Fixity.Infix y => (Output.text (p, "infix "); y)
			| Ast.Fixity.Infixr y => (Output.text (p, "infixr "); y)
			| Ast.Fixity.Nonfix => (Output.text (p, "nonfix "); NONE)
	) of
		NONE => ()
		| SOME z => (
			Output.text (p, Int.toString z)
			; Output.text (p, " ")
		)
and writeVid (p, x) = (
	writeRegion (p, Ast.Vid.region x)
	; Output.text (p, Ast.Vid.toString x)
	; Output.text (p, " ")
) and writeVids (p, v) = Vector.app (fn x => writeVid (p, x)) v
and writeFix (p, { fixity, ops }) = (
	writeFixity (p, fixity)
	; writeVids (p, ops)
) and writeFixop (p, x) = case x of
	Ast.Fixop.None => ()
	| Ast.Fixop.Op => Output.text (p, "op ")
and writeLongvidId (p, x) = (
	(*writeRegion (p, Ast.Longvid.Id.region x)
	; *)Output.text (p, Ast.Longvid.Id.toString x)
) and writeLongvid (p, x) =
	let
		val Ast.Longvid.T { strids, id } = Ast.Longvid.node x
	in
		writeStrids (p, strids)
		; writeLongvidId (p, id)
		; Output.text (p, " ")
	end
and writePatVar (p, { fixop, name }) = (
	writeFixop (p, fixop)
	; writeLongvid (p, name)
) and writeWild p = Output.text (p, "_ ")
and writePatTuple (p, v) = (
	Output.text (p, "(")
	; Vector.appSeparate (fn x => writePat (p, x), curryOutput (p, ", ")) v
	; Output.text (p, ")")
) and writeConstraint (p, x) = case x of
	NONE => ()
	| SOME y => (
		Output.text (p, ": ")
		; writeType (p, y)
	)
and writeAsPattern (p, x) = (
	Output.text (p, " as ")
	; writePat (p, x)
) and writeItem (p, (field, item)) = (
	Output.text (p, Ast.Record.Field.toString field)
	; Output.text (p, " ")
	; case item of
		Ast.Pat.Item.Vid (_, constraint, aspattern) => (
			writeConstraint (p, constraint)
			; case aspattern of
				NONE => ()
				| SOME x => writeAsPattern (p, x)
		) | Ast.Pat.Item.Field x => (
			Output.text (p, " = ")
			; writePat (p, x)
		)
) and writePatRecord (p, {flexible, items}) = (
	Output.text (p, "{ ")
	; Vector.appSeparate (
		fn x => writeItem (p, x)
		, curryOutput (p, ", ")
	) items
	; if flexible then Output.text (p, ", ...") else ()
	; Output.text (p, "}")
) and writePatList (p, v) = (
	Output.text (p, "[")
	; Vector.appSeparate (fn x => writePat (p, x), curryOutput (p, ", ")) v
	; Output.text (p, "]")
) and writePatLayered (p, {constraint, fixop, pat, var}) = (
	writeFixop (p, fixop)
	; writeVar (p, var)
	; writeConstraint (p, constraint)
	; writeAsPattern (p, pat)
) and writeBool (p, x) = Output.text (p, if x then "true " else "false ")
and writeChar (p, x) = (
	Output.text (p, "#\"")
	; Output.text (p, x)
	; Output.text (p, "\"")
) and writeInt (p, x) = (
	Output.text (p, x)
	; Output.text (p, " ")
) and writeReal (p, x) = (
	Output.text (p, x)
	; Output.text (p, " ")
) and writeWord (p, x) = Output.text (p, x)
and writeString (p, x) = (
	Output.text (p, "\"")
	; Output.text (p, x)
	; Output.text (p, "\"")
) and writeConst (p, x) = (
	writeRegion (p, Ast.Const.region x)
	; case Ast.Const.node x of
		Ast.Const.Bool y => writeBool (p, y)
		| Ast.Const.Char y => writeChar (p, y)
		| Ast.Const.Int y => writeInt (p, y)
		| Ast.Const.Real y => writeReal (p, y)
		| Ast.Const.String y => writeString (p, y)
		| Ast.Const.Word y => writeWord (p, y)
) and writePatConstraint (p, (pat, typ)) = (
	writePat (p, pat)
	; Output.text (p, ": ")
	; writeType (p, typ)
) and writePatFlatApp (p, v) = Vector.app (fn x => writePat (p, x)) v
and writePat (p, x) = (
	writeRegion (p, Ast.Pat.region x)
	; case Ast.Pat.node x of
		Ast.Pat.Const y => writeConst (p, y)
		| Ast.Pat.Constraint y => writePatConstraint (p, y)
		| Ast.Pat.FlatApp y => writePatFlatApp (p, y)
		| Ast.Pat.Layered y => writePatLayered (p, y)
		| Ast.Pat.List y => writePatList (p, y)
		| Ast.Pat.Record y => writePatRecord (p, y)
		| Ast.Pat.Tuple y => writePatTuple (p, y)
		| Ast.Pat.Var y => writePatVar (p, y)
		| Ast.Pat.Wild => writeWild p
) and writePats (p, v) = Vector.app (fn x => writePat (p, x)) v
and writeExpAndalso (p, (a, b)) = (
	writeExp (p, a)
	; Output.text (p, "andalso ")
	; writeExp (p, b)
) and writeExpApp (p, (f, x)) = (
	writeExp (p, f)
	; writeExp (p, x)
) and writeExpCase (p, (x, y)) = (
	Output.text (p, "case ")
	; writeExp (p, x)
	; Output.text (p, "of ")
	; writeMatch (p, y)
) and writeExpConstraint (p, (x, y)) = (
	writeExp (p, x)
	; Output.text (p, ": ")
	; writeType (p, y)
) and writeExpFlatApp (p, v) = Vector.app (fn x => writeExp (p, x)) v
and writePatExp (p, (pat, exp)) = (
	writePat (p, pat)
	; Output.text (p, " => ")
	; writeExp (p, exp)
) and writeMatch (p, x) = (
	writeRegion (p, Ast.Match.region x)
	; let
		val Ast.Match.T v = Ast.Match.node x
	in
		Vector.appSeparate (
			fn x => writePatExp (p, x)
			, fn () => Output.text (p, " | ")
		) v
	end
) and writeExpHandle (p, (x, y)) = (
	writeExp (p, x)
	; Output.text (p, "handle ")
	; writeMatch (p, y)
) and writeExpIf (p, (x, y, z)) = (
	Output.text (p, "if ")
	; writeExp (p, x)
	; Output.text (p, "then ")
	; writeExp (p, y)
	; Output.text (p, "else ")
	; writeExp (p, z)
) and writeExpLet (p, (x, y)) = (
	Output.text (p, "let ")
	; writeDec (p, x)
	; Output.text (p, "in ")
	; writeExp (p, y)
	; Output.text (p, "end ")
) and writeExpList (p, v) = (
	Output.text (p, "[")
	; Vector.appSeparate (
		fn x => writeExp (p, x)
		, curryOutput (p, ", ")
	) v
	; Output.text (p, "]")
) and writeExpOrelse (p, (x, y)) = (
	writeExp (p, x)
	; Output.text (p, "orelse ")
	; writeExp (p, y)
) and writeExpRaise (p, x) = (
	Output.text (p, "raise ")
	; writeExp (p, x)
) and writeExpRecord (p, x) =
	case Ast.Record.detupleOpt x of
		NONE => writeExpActualRecord (p, x)
		| SOME t => writeExpTuple (p, t)
and writeExpActualRecord (p, r) = (
	Output.text (p, "{")
	; Vector.appSeparate (
		fn (f, x) => (
			Output.text (p, Ast.Record.Field.toString f)
			; Output.text (p, " = ")
			; writeExp (p, x)
		), fn () => Output.text (p, ", ")
	) (Ast.Record.toVector r)
	; Output.text (p, "}")
) and writeExpTuple (p, v) = (
	Output.text (p, "(")
	; Vector.appSeparate (
		fn x => writeExp (p, x)
		, fn () => Output.text (p, ", ")
	) v
	; Output.text (p, ")")
) and writeField (p, x) = (
	Output.text (p, Ast.Record.Field.toString x)
	; Output.text (p, " ")
) and writeExpSelector (p, x) = (
	Output.text (p, "# ")
	; writeField (p, x)
) and writeExpSeq (p, v) = (
	Output.text (p, "(")
	; Vector.appSeparate (
		fn x => writeExp (p, x)
		, fn () => Output.text (p, "; ")
	) v
	; Output.text (p, ")")
) and writeExpVar (p, {fixop, name}) = (
	writeFixop (p, fixop)
	; writeLongvid (p, name)
) and writeExpWhile (p, {expr, test}) = (
	Output.text (p, "while ")
	; writeExp (p, test)
	; Output.text (p, "do ")
	; writeExp (p, expr)
) and writeExpFn (p, m) = (
	Output.text (p, "fn ")
	; writeMatch (p, m)
) and writeExp (p, x) = (
	writeRegion (p, Ast.Exp.region x)
	; case Ast.Exp.node x of
		Ast.Exp.Andalso y => writeExpAndalso (p, y)
		| Ast.Exp.App y => writeExpApp (p, y)
		| Ast.Exp.Case y => writeExpCase (p, y)
		| Ast.Exp.Const y => writeConst (p, y)
		| Ast.Exp.Constraint y => writeExpConstraint (p, y)
		| Ast.Exp.FlatApp y => writeExpFlatApp (p, y)
		| Ast.Exp.Fn y => writeExpFn (p, y)
		| Ast.Exp.Handle y => writeExpHandle (p, y)
		| Ast.Exp.If y => writeExpIf (p, y)
		| Ast.Exp.Let y => writeExpLet (p, y)
		| Ast.Exp.List y => writeExpList (p, y)
		| Ast.Exp.Orelse y => writeExpOrelse (p, y)
		| Ast.Exp.Raise y => writeExpRaise (p, y)
		| Ast.Exp.Record y => writeExpRecord (p, y)
		| Ast.Exp.Selector y => writeExpSelector (p, y)
		| Ast.Exp.Seq y => writeExpSeq (p, y)
		| Ast.Exp.Var y => writeExpVar (p, y)
		| Ast.Exp.While y => writeExpWhile (p, y)
) and writeClause (p, { body, pats, resultType }) = (
	writePats (p, pats)
	; writeConstraint (p, resultType)
	; Output.text (p, " = ")
	; writeExp (p, body)
) and writeFunction (p, v) =
	Vector.appSeparate (fn x => writeClause (p, x), curryOutput (p, "| ")) v
and writeFunctions (p, v) =
	Vector.appSeparate (fn x => writeFunction (p, x), curryOutput (p, "and ")) v
and writeDecFun (p, (tyvars, functions)) = (
	Output.text (p, "fun ")
	; writeTyvars (p, tyvars)
	; writeFunctions (p, functions)
) and writeAbstype (p, {datBind, body}) = (
	Output.text (p, "abstype ")
	; writeDatBind (p, datBind)
	; Output.text (p, "with ")
	; writeDec (p, body)
	; Output.text (p, "end ")
) and writeDecOpen (p, v) = (
	Output.text (p, "open ")
	; Vector.app (fn x => writeLongstrid (p, x)) v
) and writeDecSeqDec (p, v) = Vector.app (fn x => writeDec (p, x)) v
and writeDecVb (p, {pat, exp}) = (
	writePat (p, pat)
	; Output.text (p, " = ")
	; writeExp (p, exp)
) and writeDecVbs (p, v) =
	Vector.appSeparate (
		fn x => writeDecVb (p, x)
		, fn () => Output.text (p, "and ")
	) v
and writeDecRvb (p, {pat, match}) = (
	writePat (p, pat)
	; Output.text (p, " = fn ")
	; writeMatch (p, match)
) and writeDecRvbs (p, v) = (
	if Vector.length v > 0 then (
		Output.text (p, "rec ")
		; Vector.appSeparate (
			fn x => writeDecRvb (p, x)
			, fn () => Output.text (p, "and ")
		) v
	) else ()
) and writeDecVal (p, {tyvars, rvbs, vbs}) = (
	Output.text (p, "val ")
	; writeTyvars (p, tyvars)
	; writeDecVbs (p, vbs)
	; writeDecRvbs (p, rvbs)
) and writeDec (p, x) = (
	writeRegion (p, Ast.Dec.region x)
	; case Ast.Dec.node x of
		Ast.Dec.Abstype y => writeAbstype (p, y)
		| Ast.Dec.Datatype y => writeDatatypeRhs (p, y)
		| Ast.Dec.Exception y => writeDecExceptions (p, y)
		| Ast.Dec.Fix y => writeFix (p, y)
		| Ast.Dec.Fun y => writeDecFun (p, y)
		| Ast.Dec.IoDatatype _ => raise NotTransformed
		| Ast.Dec.IoType _ => raise NotTransformed
		| Ast.Dec.Local y => writeDecLocal (p, y)
		| Ast.Dec.Open y => writeDecOpen (p, y)
		| Ast.Dec.SeqDec y => writeDecSeqDec (p, y)
		| Ast.Dec.Type y => writeTypBind (p, y)
		| Ast.Dec.Val y => writeDecVal (p, y)
) and writeStrdecSeq (p, x) = app (fn y => writeStrdec (p, y)) x
and writeSigConst (p, x) = case x of
	Ast.SigConst.None => ()
	| Ast.SigConst.Opaque x => (
		Output.text (p, ":> ")
		; writeSigexp (p, x)
	) | Ast.SigConst.Transparent x => (
		Output.text (p, ": ")
		; writeSigexp (p, x)
	)
and writeFctid (p, x) = (
	writeRegion (p, Ast.Fctid.region x)
	; Output.text (p, Ast.Fctid.toString x)
	; Output.text (p, " ")
) and writeStrexpApp (p, (id, strexp)) = (
	writeFctid (p, id)
	; Output.text (p, "(")
	; writeStrexp (p, strexp)
	; Output.text (p, ") ")
) and writeStrexpConstrained (p, (strexp, sigconst)) = (
	writeStrexp (p, strexp)
	; writeSigConst (p, sigconst)
) and writeStrexpLet (p, (strdec, strexp)) = (
	Output.text (p, "let ")
	; writeStrdec (p, strdec)
	; Output.text (p, "in ")
	; writeStrexp (p, strexp)
	; Output.text (p, "end ")
) and writeStrexpStruct (p, strdec) = (
	Output.text (p, "struct ")
	; writeStrdec (p, strdec)
	; Output.text (p, "end ")
) and writeStrexpVar (p, longstrid) = writeLongstrid (p, longstrid)
and writeStrexp (p, x) = (
	writeRegion (p, Ast.Strexp.region x)
	; case Ast.Strexp.node x of
		Ast.Strexp.App y => writeStrexpApp (p, y)
		| Ast.Strexp.Constrained y => writeStrexpConstrained (p, y)
		| Ast.Strexp.Let y => writeStrexpLet (p, y)
		| Ast.Strexp.Struct y => writeStrexpStruct (p, y)
		| Ast.Strexp.Var y => writeStrexpVar (p, y)
) and writeStrdecStructure (p, {constraint, def, name}) = (
	writeStrid (p, name)
	; writeSigConst (p, constraint)
	; Output.text (p, " = ")
	; writeStrexp (p, def)
)
and writeStrdecStructures (p, v) = (
	Output.text (p, "structure ")
	; Vector.appSeparate (
		fn x => writeStrdecStructure (p, x)
		, fn () => Output.text (p, "and ")
	) v
) and writeStrdec (p, x) = (
	writeRegion (p, Ast.Strdec.region x)
	; case Ast.Strdec.node x of
		Ast.Strdec.Core y => writeDec (p, y)
		| Ast.Strdec.Local y => writeStrdecLocal (p, y)
		| Ast.Strdec.Seq y => writeStrdecSeq (p, y)
		| Ast.Strdec.Structure y => writeStrdecStructures (p, y)
) and writeFctArgStructure (p, (strid, sigexp)) = (
	writeStrid (p, strid)
	; Output.text (p, ": ")
	; writeSigexp (p, sigexp)
) and writeFctArg (p, x) = (
	writeRegion (p, Ast.FctArg.region x)
	; case Ast.FctArg.node x of
		Ast.FctArg.Structure y => writeFctArgStructure (p, y)
		| Ast.FctArg.Spec y => writeSpec (p, y)
) and writeFunctor (p, {name, result, arg, body}) = (
	writeFctid (p, name)
	; Output.text (p, "(")
	; writeFctArg (p, arg)
	; Output.text (p, ")")
	; writeSigConst (p, result)
	; Output.text (p, " = ")
	; writeStrexp (p, body)
) and writeFunctors (p, v) = (
	Output.text (p, "functor ")
	; Vector.appSeparate (
		fn x => writeFunctor (p, x)
		, fn () => Output.text (p, "and ")
	) v
)
fun writeTopdec (p, w) =
	let
		val (node, region) = Ast.Topdec.dest w
	in
		writeRegion (p, region)
		; case node of
			Ast.Topdec.Functor y => writeFunctors (p, y)
			| Ast.Topdec.Signature y => writeSignature (p, y)
			| Ast.Topdec.Strdec y => writeStrdec (p, y)
	end
fun writeProgram (inputName, outputName, p, Ast.Program.T x) =
	let
		val p = Output.fromOutstream {
			inputName = inputName
			, outputName = outputName
			, outstream = p
		}
	in
		app (fn y => app (fn z => writeTopdec (p, z)) y) x
		; Output.text (p, "\n")
	end

end (* functor Write *)
