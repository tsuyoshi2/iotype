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
functor Transform (structure Ast: AST) :> sig
	exception Arrow of Region.t
	val transformProgram: Ast.Program.t -> Ast.Program.t
end = struct
	exception Arrow of Region.t
	structure String = struct
		open String
		fun capitalize string = CharVector.update (
			string
			, 0
			, Char.toUpper (CharVector.sub (string, 0))
		)
		fun prefixCapitalize prefix string = prefix ^ (capitalize string)
		(*
			We need the identifiers p and x to be available.
		*)
		fun isOperator s = case s of
			"" => false
			| _ => Char.contains "-!%&$+/:<=>?@~`^|#*\\" (String.sub (s, 0))
		fun notReserved s =
			if isOperator s then s
			else "y" ^ s
	end
	structure Ast = struct
		open Ast
		structure Vid = struct
			open Vid
			fun fromTycon transform tycon =
				fromSymbol (
					Symbol.fromString (
						transform (Tycon.toString tycon)
					), Region.bogus
				)
			fun fromTyvar transform tyvar =
				fromSymbol (
					Symbol.fromString (
						transform (Tyvar.name tyvar)
					), Region.bogus
				)
			fun fromCon transform con =
				fromSymbol (
					Symbol.fromString (
						transform (Con.toString con)
					), Region.bogus
				)
			fun fromField transform field =
				fromSymbol (
					case field of
						Record.Field.Int i =>
							Symbol.fromString (
								transform (Int.toString i)
							)
						| Record.Field.Symbol s =>
							Symbol.fromString (
								transform (
									Symbol.toString s
								)
							)
					, Region.bogus
				)
		end
		structure Longvid = struct
			open Longvid
			fun fromString string =
				short (
					Vid.fromSymbol (
						Symbol.fromString string
						, Region.bogus
					)
				)
			fun fromLongtycon transform longtycon =
				let
					val (structures, variable) =
						Longtycon.split longtycon
				in
					long (structures, Vid.fromTycon transform variable)
				end
			fun fromTycon transform tycon = short (Vid.fromTycon transform tycon)
			fun fromTyvar transform tyvar = short (Vid.fromTyvar transform tyvar)
			fun fromCon transform con = short (Vid.fromCon transform con)
			fun fromField transform field = short (Vid.fromField transform field)
		end
		structure Pat = struct
			open Pat
			val varFromString = longvid o Longvid.fromString
			val none = varFromString "NONE"
			fun some name = makeRegion (
				FlatApp (Vector.fromList [
					varFromString "SOME"
					, name
				]), Region.bogus
			)
			fun varFromTyvar transform tyvar =
				makeRegion (
					Var {
						fixop = Fixop.None
						, name = Longvid.fromTyvar transform tyvar
					}, Region.bogus
				)
			fun varFromTycon transform tycon =
				makeRegion (
					Var {
						fixop = Fixop.None
						, name = Longvid.fromTycon transform tycon
					}, Region.bogus
				)
			fun varFromField transform field =
				makeRegion (
					Var {
						fixop = Fixop.None
						, name = Longvid.fromField transform field
					}, Region.bogus
				)
		end
		structure Exp = struct
			open Exp
			val varFromString = longvid o Longvid.fromString
			fun varFromLongtycon (transform: string -> string) longtycon =
				longvid (Longvid.fromLongtycon transform longtycon)
			fun varFromCon con = makeRegion (
				Var {
					fixop = Fixop.None
					, name = Longvid.fromCon (fn x => x) con
				}, Region.bogus
			)
			fun varFromField transform field = makeRegion (
				Var {
					fixop = Fixop.None
					, name = Longvid.fromField transform field
				}, Region.bogus
			)
			fun fn1 x = fnn (Vector.fromList [x])
			fun fnX (value, body) =
				app (
					tuple (Vector.fromList [fn1 (Pat.varFromString "x", body)])
					, value
				)
			fun seq v = makeRegion (
				Seq v
				, if Vector.length v = 0 then Region.bogus
				else Region.append (
					region (Vector.sub (v, 0))
					, region (Vector.sub (v, Vector.length v - 1))
				)
			)
			fun caseOpt (caseExpression, noneExpression, someName, someExpression) =
				makeRegion (
					Case (
						caseExpression
						, Match.makeRegion (
							Match.T (Vector.fromList [
								(Pat.none, noneExpression)
								, (
									Pat.some someName
									, someExpression
								)
							]), Region.bogus
						)
					), Region.bogus
				)
			val none = varFromString "NONE"
			fun some x = app (
				varFromString "SOME"
				, x
			)
		end
		structure Var = struct
			open Var
			fun fromString string = fromSymbol (
				Symbol.fromString string
				, Region.bogus
			)
			fun fromTycon transform tycon =
				fromString (transform (Tycon.toString tycon))
		end
		structure Longtycon = struct
			open Longtycon
			fun fromString string =
				let
					val list = String.fields (fn c => #"." = c) string
				in
					fromSymbols (
						map Symbol.fromString list
						, Region.bogus
					)
				end
		end
		structure Type = struct
			open Type
			fun longCon (string, arguments) =
				makeRegion (
					Con (Longtycon.fromString string, arguments)
					, Region.bogus
				)
		end
		structure Spec = struct
			open Spec
			val empty = makeRegion (Empty, Region.bogus)
			fun seq (x as (y, z)) = makeRegion (
				Seq x
				, Region.append (region y, region z)
			)
		end
		structure Match = struct
			open Match
			fun make v = makeRegion (T v, Region.bogus)
		end
		structure TypBind = struct
			open TypBind
			fun isEmpty t =
				let
					val T v = node t
				in
					Vector.length v = 0
				end
		end
	end (* structure Ast *)
	structure Write :> sig
		val decFromType: Ast.TypBind.t -> Ast.Dec.t
		val decFromDatBind: Ast.DatBind.t -> Ast.Dec.t
		val specFromDatBind: Ast.DatBind.t -> Ast.Spec.t
		val specFromTypeDefs: Ast.TypBind.t -> Ast.Spec.t
		val specFromType:
			{tycon: Ast.Tycon.t, tyvars: Ast.Tyvar.t vector} vector -> Ast.Spec.t
	end = struct
		fun expSelector field =
			Ast.Exp.makeRegion (Ast.Exp.Selector field, Region.bogus)
		fun expFromField (field, type') =
			Ast.Exp.fnX (
				Ast.Exp.tuple (Vector.fromList [
					Ast.Exp.app (expSelector field, Ast.Exp.varFromString "x")
				]), expFromType type'
			)
		and expFromRecord rType =
			Ast.Exp.makeRegion (
				Ast.Exp.Seq (
					Vector.map (fn (field, type') =>
						expFromField (field, type')
					) (Ast.SortedRecord.toVector rType)
				), Region.bogus
			)
		and fnFromCon (constructor, arguments) =
			let
				val base = Ast.Exp.varFromLongtycon
					(String.prefixCapitalize "write")
					constructor
			in
				if Vector.length arguments = 0 then base
				else Ast.Exp.app (
					base
					, Ast.Exp.tuple (Vector.map fnFromType arguments)
				)
			end
		and fnFromType t =
			case Ast.Type.node t of
				Ast.Type.Con c => fnFromCon c
				| _ => Ast.Exp.fn1 (
					Ast.Pat.tuple (Vector.fromList [
						Ast.Pat.varFromString "p"
						, Ast.Pat.varFromString "x"
					]), expFromType t
				)
		and expFromCon (c, arguments) =
			Ast.Exp.app (
				fnFromCon (c, arguments)
				, Ast.Exp.tuple (Vector.fromList [
					Ast.Exp.varFromString "p"
					, Ast.Exp.varFromString "x"
				])
			)
		and expFromVar v =
			Ast.Exp.app (
				Ast.Exp.varFromString (String.notReserved (Ast.Tyvar.name v))
				, Ast.Exp.tuple (Vector.fromList [
					Ast.Exp.varFromString "p"
					, Ast.Exp.varFromString "x"
				])
			)
		and expFromType t = case Ast.Type.node t of
			Ast.Type.Arrow _ => raise Arrow (Ast.Type.region t)
			| Ast.Type.Con c => expFromCon c
			| Ast.Type.Group g => expFromType g
			| Ast.Type.Record r => expFromRecord r
			| Ast.Type.Var v => expFromVar v
		and patVarFromCon con =
			Ast.Pat.makeRegion (
				Ast.Pat.Var {
					fixop = Ast.Fixop.None
					, name = Ast.Longvid.fromCon (fn x => x) con
				}, Region.bogus
			)
		and clausesFromTypBind {tyvars, tycon, def} =
			let
				val name = Ast.Pat.varFromTycon
					(String.prefixCapitalize "write")
					tycon
				val px = Ast.Pat.tuple (Vector.fromList [
					Ast.Pat.varFromString "p"
					, Ast.Pat.constraint (
						Ast.Pat.varFromString "x"
						, Ast.Type.con (
							tycon
							, Vector.map Ast.Type.var tyvars
						)
					)
				])
				val pats = Vector.fromList (
					if Vector.length tyvars = 0 then [name, px]
					else [
						name
						, Ast.Pat.tuple (Vector.map
							(Ast.Pat.varFromTyvar String.notReserved)
							tyvars
						), px
					]
				)
			in
				Vector.fromList [{
					pats = pats
					, resultType = NONE
					, body = expFromType def
				}]
			end
		and decFromType bindings =
			let
				val Ast.TypBind.T v = Ast.TypBind.node bindings
			in
				Ast.Dec.makeRegion (
					Ast.Dec.Fun (
						Vector.fromList []
						, Vector.map clausesFromTypBind v
					), Region.bogus
				)
			end
		and constFromInt i =
			Ast.Const.makeRegion (
				Ast.Const.Int (Int.toString i)
				, Region.bogus
			)
		and expFromInt i =
			Ast.Exp.app (
				Ast.Exp.varFromString "Int16.writeInt"
				, Ast.Exp.tuple (Vector.fromList [
					Ast.Exp.varFromString "p"
					, Ast.Exp.const (constFromInt i)
				])
			)
		and patFlatApp l =
			Ast.Pat.makeRegion (
				Ast.Pat.FlatApp (Vector.fromList l)
				, Region.bogus
			)
		and matchFromCons v = Ast.Match.make (
			Vector.mapi (fn (i, (con, t)) => (case t of
				NONE => (
					patVarFromCon con
					, expFromInt i
				) | SOME x => (
					patFlatApp [
						patVarFromCon con
						, Ast.Pat.varFromString "x"
					], Ast.Exp.seq (Vector.fromList [
						expFromInt i
						, expFromType x
					])
				)
			)) v
		)
		and expFromCons v =
			Ast.Exp.makeRegion (
				Ast.Exp.Case (
					Ast.Exp.varFromString "x"
					, matchFromCons v
				), Region.bogus
			)
		and clausesFromDatatypes {tyvars, tycon, cons} =
			let
				val name = Ast.Pat.varFromTycon
					(String.prefixCapitalize "write")
					tycon
				val px = Ast.Pat.tuple (Vector.fromList [
					Ast.Pat.varFromString "p", Ast.Pat.varFromString "x"
				])
			in
				Vector.fromList [{
					pats = Vector.fromList (
						if Vector.length tyvars = 0 then [name, px]
						else [
							name
							, Ast.Pat.tuple (Vector.map
								(Ast.Pat.varFromTyvar String.notReserved)
								tyvars
							), px
						]
					), resultType = NONE
					, body = expFromCons cons
				}]
			end
		and decFromDatBind x =
			let
				val Ast.DatBind.T {datatypes, withtypes} = Ast.DatBind.node x
				val dec =
					Ast.Dec.Fun (
						Vector.fromList []
						, Vector.map
							clausesFromDatatypes
							datatypes
					)
			in
				Ast.Dec.makeRegion (
					if Ast.TypBind.isEmpty withtypes then dec
					else Ast.Dec.Local (
						decFromType withtypes
						, Ast.Dec.makeRegion (dec, Region.bogus)
					), Region.bogus
				)
			end
		and clauseFromRepl {lhs, rhs} =
			let
				val name = Ast.Pat.varFromTycon (String.prefixCapitalize "write") lhs
				val px = Ast.Pat.tuple (Vector.fromList [
					Ast.Pat.varFromString "p", Ast.Pat.varFromString "x"
				])
			in
				{
					pats = Vector.fromList [name, px]
					, resultType = NONE
					, body = expFromCon rhs
				}
			end
		and specFromElements v = Ast.Spec.makeRegion (Ast.Spec.Val v, Region.bogus)
		and specFromDatBind x =
			let
				val Ast.DatBind.T {datatypes, withtypes} = Ast.DatBind.node x
			in
				specFromElements (
					Vector.map (fn {cons, tycon, tyvars} =>
						specElement (tyvars, tycon)
					) datatypes
				)
			end
		and specFromTypeDefs x =
			let
				val Ast.TypBind.T v = Ast.TypBind.node x
			in
				specFromElements (
					Vector.map (fn {def, tycon, tyvars} =>
						specElement (tyvars, tycon)
					) v
				)
			end
		and specFromType v = specFromElements (
			Vector.map (fn {tycon, tyvars} =>
				specElement (tyvars, tycon)
			) v
		)
		and specElement (tyvars, tycon) =
			let
				val name = Ast.Var.fromTycon (String.prefixCapitalize "write") tycon
				val px = Ast.Type.tuple (Vector.fromList [
					Ast.Type.longCon ("BinIO.outstream", Vector.fromList nil)
					, Ast.Type.con (tycon, Vector.map Ast.Type.var tyvars)
				])
				val pxToUnit = Ast.Type.arrow (px, Ast.Type.unit)
				fun writers () = Ast.Type.tuple (Vector.map (fn x =>
					Ast.Type.arrow (
						Ast.Type.tuple (Vector.fromList [
							Ast.Type.longCon (
								"BinIO.outstream"
								, Vector.fromList nil
							), Ast.Type.var x
						]), Ast.Type.unit
					)
				) tyvars)
			in
				(
					name
					, if Vector.length tyvars = 0 then pxToUnit
					else Ast.Type.arrow (writers (), pxToUnit)
				)
			end
	end (* structure Write *)
	structure Read :> sig
		val decFromType: Ast.TypBind.t -> Ast.Dec.t
		val decFromDatBind: Ast.DatBind.t -> Ast.Dec.t
		val specFromDatBind: Ast.DatBind.t -> Ast.Spec.t
		val specFromTypeDefs: Ast.TypBind.t -> Ast.Spec.t
		val specFromType:
			{tycon: Ast.Tycon.t, tyvars: Ast.Tyvar.t vector} vector -> Ast.Spec.t
	end = struct
		fun expFromType t = case Ast.Type.node t of
			Ast.Type.Arrow _ => raise Arrow (Ast.Type.region t)
			| Ast.Type.Con c => expFromCon c
			| Ast.Type.Group g => expFromType g
			| Ast.Type.Record r => expFromRecord r
			| Ast.Type.Var v => expFromVar v
		and clausesFromTypBind {tyvars, tycon, def} =
			let
				val name = Ast.Pat.varFromTycon
					(String.prefixCapitalize "read")
					tycon
				val p = Ast.Pat.varFromString "p"
				val pats = Vector.fromList (
					if Vector.length tyvars = 0 then [name, p]
					else [
						name
						, Ast.Pat.tuple (Vector.map
							(Ast.Pat.varFromTyvar String.notReserved)
							tyvars
						), p
					]
				)
			in
				Vector.fromList [{
					pats = pats
					, resultType = NONE
					, body = expFromType def
				}]
			end
		and decFromType bindings =
			let
				val Ast.TypBind.T v = Ast.TypBind.node bindings
			in
				Ast.Dec.makeRegion (
					Ast.Dec.Fun (
						Vector.fromList nil
						, Vector.map clausesFromTypBind v
					), Region.bogus
				)
			end
		and expFromCon (c, arguments) =
			Ast.Exp.app (
				fnFromCon (c, arguments)
				, Ast.Exp.varFromString "p"
			)
		and fnFromCon (constructor, arguments) =
			let
				val base = Ast.Exp.varFromLongtycon
					(String.prefixCapitalize "read")
					constructor
			in
				if Vector.length arguments = 0 then base
				else Ast.Exp.app (
					base
					, Ast.Exp.tuple (Vector.map fnFromType arguments)
				)
			end
		and fnFromType t =
			case Ast.Type.node t of
				Ast.Type.Con c => fnFromCon c
				| _ => Ast.Exp.fn1 (
					Ast.Pat.varFromString "p"
					, expFromType t
				)
		and expFromRecord rType =
			let
				val vector = Ast.SortedRecord.toVector rType
				val create = Ast.Exp.makeRegion (
					Ast.Exp.Record (
						Ast.Record.fromVector (
							Vector.map (fn (field, _) => (
								field
								, Ast.Exp.varFromField
									String.notReserved
									field
							)) vector
						)
					), Region.bogus
				)
			in
				Vector.foldr (fn ((field, type'), expression) =>
					Ast.Exp.caseOpt (
						expFromType type'
						, Ast.Exp.none
						, Ast.Pat.varFromField String.notReserved field
						, Ast.Exp.tuple (Vector.fromList [expression])
					)
				) (Ast.Exp.some create) vector
			end
		and expFromVar v =
			Ast.Exp.app (
				Ast.Exp.varFromString (String.notReserved (Ast.Tyvar.name v))
				, Ast.Exp.varFromString "p"
			)
		and decFromDatBind x =
			let
				val Ast.DatBind.T {datatypes, withtypes} = Ast.DatBind.node x
				val dec = 
					Ast.Dec.Fun (
						Vector.fromList []
						, Vector.map
							clausesFromDatatypes
							datatypes
					)
			in
				Ast.Dec.makeRegion (
					if Ast.TypBind.isEmpty withtypes then dec
					else Ast.Dec.Local (
						decFromType withtypes
						, Ast.Dec.makeRegion (dec, Region.bogus)
					), Region.bogus
				)
			end
		and clausesFromDatatypes {tyvars, tycon, cons} =
			let
				val name = Ast.Pat.varFromTycon
					(String.prefixCapitalize "read")
					tycon
				val p = Ast.Pat.varFromString "p"
			in
				Vector.fromList [{
					pats = Vector.fromList (
						if Vector.length tyvars = 0 then [name, p]
						else [
							name
							, Ast.Pat.tuple (Vector.map
								(
									Ast.Pat.varFromTyvar
									String.notReserved
								) tyvars
							), p
						]
					), resultType = NONE
					, body = expFromCons cons
				}]
			end
		and constFromInt i =
			Ast.Const.makeRegion (
				Ast.Const.Int (Int.toString i)
				, Region.bogus
			)
		and patFromInt i =
			Ast.Pat.makeRegion (
				Ast.Pat.Const (constFromInt i)
				, Region.bogus
			)
		and expFromCons v =
			Ast.Exp.makeRegion (
				Ast.Exp.Case (
					Ast.Exp.app (
						Ast.Exp.varFromString "Int16.readInt"
						, Ast.Exp.varFromString "p"
					), matchFromCons v
				), Region.bogus
			)
		and choiceFromCon (i, con, t) = (
			Ast.Pat.some (patFromInt i)
			, case t of
				NONE => Ast.Exp.some (Ast.Exp.varFromCon con)
				| SOME x => Ast.Exp.tuple (Vector.fromList [Ast.Exp.caseOpt (
					expFromType x
					, Ast.Exp.none
					, Ast.Pat.varFromString "x"
					, Ast.Exp.some (
						Ast.Exp.tuple (Vector.fromList [
							Ast.Exp.app (
								Ast.Exp.varFromCon con
								, Ast.Exp.varFromString "x"
							)
						])
					)
				)])
		)
		and matchFromCons v =
			let
				val n = Vector.length v
			in
				Ast.Match.make (
					Vector.tabulate (
						n + 1
						, fn i =>
							if i = n then (Ast.Pat.wild, Ast.Exp.none)
							else
								let
									val (con, t) =
										Vector.sub (v, i)
								in
									choiceFromCon (i, con, t)
								end
					)
				)
			end
		and specFromDatBind x =
			let
				val Ast.DatBind.T {datatypes, withtypes} = Ast.DatBind.node x
			in
				specFromElements (
					Vector.map (fn {cons, tycon, tyvars} =>
						specElement (tyvars, tycon)
					) datatypes
				)
			end
		and specFromElements v = Ast.Spec.makeRegion (Ast.Spec.Val v, Region.bogus)
		and specFromTypeDefs x =
			let
				val Ast.TypBind.T v = Ast.TypBind.node x
			in
				specFromElements (
					Vector.map (fn {def, tycon, tyvars} =>
						specElement (tyvars, tycon)
					) v
				)
			end
		and specFromType v = specFromElements (
			Vector.map (fn {tycon, tyvars} =>
				specElement (tyvars, tycon)
			) v
		)
		and specElement (tyvars, tycon) =
			let
				val name = Ast.Var.fromTycon (String.prefixCapitalize "read") tycon
				val p = Ast.Type.longCon ("BinIO.instream", Vector.fromList nil)
				val x = Ast.Type.longCon ("option", Vector.fromList [
					Ast.Type.con (tycon, Vector.map Ast.Type.var tyvars)
				])
				val pToX = Ast.Type.arrow (p, x)
				fun readers () = Ast.Type.tuple (Vector.map (fn x =>
					Ast.Type.arrow (
						Ast.Type.longCon (
							"BinIO.instream"
							, Vector.fromList nil
						), Ast.Type.longCon (
							"option"
							, Vector.fromList [Ast.Type.var x]
						)
					)
				) tyvars)
			in
				(
					name
					, if Vector.length tyvars = 0 then pToX
					else Ast.Type.arrow (readers (), pToX)
				)
			end
	end (* structure Read *)

	structure Ast = struct
		open Ast
		structure DatatypeRhs = struct
			open DatatypeRhs
			fun fromDatBind x = makeRegion (DatBind x, DatBind.region x)
		end
	end
	fun transformExp x = Ast.Exp.makeRegion (
		case Ast.Exp.node x of
			Ast.Exp.Andalso (y, z) => Ast.Exp.Andalso (transformExp y, transformExp z)
			| Ast.Exp.App (y, z) => Ast.Exp.App (transformExp y, transformExp z)
			| Ast.Exp.Case (y, z) => Ast.Exp.Case (transformExp y, transformMatch z)
			| y as Ast.Exp.Const _ => y
			| Ast.Exp.Constraint (y, z) => Ast.Exp.Constraint (transformExp y, z)
			| Ast.Exp.FlatApp y => Ast.Exp.FlatApp (Vector.map transformExp y)
			| Ast.Exp.Fn y => Ast.Exp.Fn (transformMatch y)
			| Ast.Exp.Handle (y, z) => Ast.Exp.Handle (transformExp y, transformMatch z)
			| Ast.Exp.If (y, z, w) => Ast.Exp.If (
				transformExp y
				, transformExp z
				, transformExp w
			) | Ast.Exp.Let (y, z) => Ast.Exp.Let (transformDec y, transformExp z)
			| Ast.Exp.List y => Ast.Exp.List (Vector.map transformExp y)
			| Ast.Exp.Orelse (y, z) => Ast.Exp.Orelse (transformExp y, transformExp z)
			| Ast.Exp.Raise y => Ast.Exp.Raise (transformExp y)
			| Ast.Exp.Record y => Ast.Exp.Record (Ast.Record.map (y, transformExp))
			| y as Ast.Exp.Selector _ => y
			| Ast.Exp.Seq y => Ast.Exp.Seq (Vector.map transformExp y)
			| y as Ast.Exp.Var _ => y
			| Ast.Exp.While {expr, test} => Ast.Exp.While {
				expr = transformExp expr
				, test = transformExp test
			}
		, Ast.Exp.region x
	)
	and transformMatch x = Ast.Match.makeRegion (
		let
			val Ast.Match.T v = Ast.Match.node x
		in
			Ast.Match.T (
				Vector.map (fn (pat, exp) =>
					(pat, transformExp exp)
				) v
			)
		end
		, Ast.Match.region x
	)
	and transformSpec x = case Ast.Spec.node x of
		Ast.Spec.Datatype _ => x
		| Ast.Spec.Eqtype _ => x
		| Ast.Spec.Empty => x
		| Ast.Spec.Exception _ => x
		| Ast.Spec.IncludeSigexp y => Ast.Spec.makeRegion (
			Ast.Spec.IncludeSigexp (transformSigexp y)
			, Ast.Spec.region x
		) | Ast.Spec.IncludeSigids _ => x
		| Ast.Spec.IoDatatype y => Ast.Spec.makeRegion (
			Ast.Spec.Seq (
				Ast.Spec.makeRegion (
					Ast.Spec.Datatype (Ast.DatatypeRhs.fromDatBind y)
					, Ast.Spec.region x
				), Ast.Spec.makeRegion (
					Ast.Spec.Seq (
						Write.specFromDatBind y
						, Read.specFromDatBind y
					), Region.bogus
				)
			), Ast.Spec.region x
		) | Ast.Spec.IoEqtype y => Ast.Spec.makeRegion (
			Ast.Spec.Seq (
				Ast.Spec.makeRegion (
					Ast.Spec.Eqtype y
					, Ast.Spec.region x
				), Ast.Spec.makeRegion (
					Ast.Spec.Seq (
						Write.specFromType y
						, Read.specFromType y
					), Region.bogus
				)
			), Ast.Spec.region x
		) | Ast.Spec.IoType y => Ast.Spec.makeRegion (
			Ast.Spec.Seq (
				Ast.Spec.makeRegion (
					Ast.Spec.Type y
					, Ast.Spec.region x
				), Ast.Spec.makeRegion (
					Ast.Spec.Seq (
						Write.specFromType y
						, Read.specFromType y
					), Region.bogus
				)
			), Ast.Spec.region x
		) | Ast.Spec.IoTypeDefs y => Ast.Spec.makeRegion (
			Ast.Spec.Seq (
				Ast.Spec.makeRegion (
					Ast.Spec.TypeDefs y
					, Ast.Spec.region x
				), Ast.Spec.makeRegion (
					Ast.Spec.Seq (
						Write.specFromTypeDefs y
						, Read.specFromTypeDefs y
					), Region.bogus
				)
			), Ast.Spec.region x
		) | Ast.Spec.Seq (y, z) => Ast.Spec.makeRegion (
			Ast.Spec.Seq (transformSpec y, transformSpec z)
			, Ast.Spec.region x
		) | Ast.Spec.Sharing _ => x
		| Ast.Spec.Structure v => Ast.Spec.makeRegion (
			Ast.Spec.Structure (
				Vector.map (fn (y, z) => (y, transformSigexp z)) v
			), Ast.Spec.region x
		) | Ast.Spec.Type _ => x
		| Ast.Spec.TypeDefs _ => x
		| Ast.Spec.Val _ => x
	and transformSigexp x = case Ast.Sigexp.node x of
		Ast.Sigexp.Spec y => Ast.Sigexp.makeRegion (
			Ast.Sigexp.Spec (transformSpec y)
			, Ast.Sigexp.region x
		) | Ast.Sigexp.Var _ => x
		| Ast.Sigexp.Where _ => x
	and transformSigConst x = case x of
		Ast.SigConst.None => x
		| Ast.SigConst.Opaque y => Ast.SigConst.Opaque (transformSigexp y)
		| Ast.SigConst.Transparent y => Ast.SigConst.Transparent (transformSigexp y)
	and transformStrexp x = Ast.Strexp.makeRegion (
		case Ast.Strexp.node x of
			Ast.Strexp.App (y, z) => Ast.Strexp.App (y, transformStrexp z)
			| Ast.Strexp.Constrained (y, z) => Ast.Strexp.Constrained (
				transformStrexp y
				, transformSigConst z
			) | Ast.Strexp.Let (y, z) => Ast.Strexp.Let (
				transformStrdec y
				, transformStrexp z
			) | Ast.Strexp.Struct y => Ast.Strexp.Struct (transformStrdec y)
			| y as Ast.Strexp.Var _ => y
		, Ast.Strexp.region x
	)
	and transformAbstype {datBind, body} = {datBind = datBind, body = transformDec body}
	and transformDecFun (x, y) = (
		x,
		Vector.map (fn z =>
			Vector.map (fn {body, pats, resultType} => {
				body = transformExp body
				, pats = pats
				, resultType = resultType
			}) z
		) y
	)
	and transformDecIoDatatype x =
		Ast.Dec.SeqDec (Vector.fromList [
			Ast.Dec.makeRegion (
				Ast.Dec.Datatype (Ast.DatatypeRhs.fromDatBind x)
				, Ast.DatBind.region x
			), Write.decFromDatBind x
			, Read.decFromDatBind x
		])
	and transformDecIoType (x, region) = Ast.Dec.makeRegion (
		Ast.Dec.SeqDec (Vector.fromList [
			Ast.Dec.makeRegion (Ast.Dec.Type x, region)
			, Write.decFromType x
			, Read.decFromType x
		]), region
	)
	and transformDecSeqDec v = Vector.map transformDec v
	and transformRvbs v = Vector.map (fn {match, pat} => {
		match = transformMatch match
		, pat = pat
	}) v
	and transformVbs v = Vector.map (fn {exp, pat} => {
		exp = transformExp exp
		, pat = pat
	}) v
	and transformDecVal {rvbs, tyvars, vbs} = {
		rvbs = transformRvbs rvbs
		, tyvars = tyvars
		, vbs = transformVbs vbs
	}
	and transformDecLocal (x, y) = (transformDec x, transformDec y)
	and transformDec x = case Ast.Dec.node x of
		Ast.Dec.Abstype y => Ast.Dec.makeRegion (
			Ast.Dec.Abstype (transformAbstype y)
			, Ast.Dec.region x
		) | Ast.Dec.Datatype _ => x
		| Ast.Dec.Exception _ => x
		| Ast.Dec.Fix _ => x
		| Ast.Dec.Fun y => Ast.Dec.makeRegion (
			Ast.Dec.Fun (transformDecFun y)
			, Ast.Dec.region x
		) | Ast.Dec.IoDatatype y => Ast.Dec.makeRegion (
			transformDecIoDatatype y
			, Ast.Dec.region x
		) | Ast.Dec.IoType y => transformDecIoType (y, Ast.Dec.region x)
		| Ast.Dec.Local y => Ast.Dec.makeRegion (
			Ast.Dec.Local (transformDecLocal y)
			, Ast.Dec.region x
		) | Ast.Dec.Open _ => x
		| Ast.Dec.SeqDec y => Ast.Dec.makeRegion (
			Ast.Dec.SeqDec (transformDecSeqDec y)
			, Ast.Dec.region x
		) | Ast.Dec.Type _ => x
		| Ast.Dec.Val y => Ast.Dec.makeRegion (
			Ast.Dec.Val (transformDecVal y)
			, Ast.Dec.region x
		)
	and transformStrdecLocal (a, b) = (transformStrdec a, transformStrdec b)
	and transformStrdecSeq l = map transformStrdec l
	and transformStrdecStructure v = 
		Vector.map (fn {constraint, def, name} => {
			constraint = transformSigConst constraint
			, def = transformStrexp def
			, name = name
		}) v
	and transformStrdec x = Ast.Strdec.makeRegion (
		case Ast.Strdec.node x of
			Ast.Strdec.Core y => Ast.Strdec.Core (transformDec y)
			| Ast.Strdec.Local y => Ast.Strdec.Local (transformStrdecLocal y)
			| Ast.Strdec.Seq y => Ast.Strdec.Seq (transformStrdecSeq y)
			| Ast.Strdec.Structure y => Ast.Strdec.Structure (transformStrdecStructure y)
		, Ast.Strdec.region x
	)
	and transformSignature v = Vector.map (fn (sigid, sigexp) =>
		(sigid, transformSigexp sigexp)
	) v
	and transformFctArg x = Ast.FctArg.makeRegion (
		case Ast.FctArg.node x of
			Ast.FctArg.Structure (y, z) => Ast.FctArg.Structure (y, transformSigexp z)
			| Ast.FctArg.Spec y => Ast.FctArg.Spec (transformSpec y)
		, Ast.FctArg.region x
	)
	and transformFunctor x = Vector.map (fn {arg, body, name, result} => {
		arg = transformFctArg arg
		, body = transformStrexp body
		, name = name
		, result = transformSigConst result
	}) x
	and transformTopdec x = Ast.Topdec.makeRegion (
		case Ast.Topdec.node x of
			Ast.Topdec.Functor y => Ast.Topdec.Functor (transformFunctor y)
			| Ast.Topdec.Signature y =>
				 Ast.Topdec.Signature (transformSignature y)
			| Ast.Topdec.Strdec y => Ast.Topdec.Strdec (transformStrdec y)
		, Ast.Topdec.region x
	)
	fun transformProgram (Ast.Program.T x) = Ast.Program.T (map (map transformTopdec) x)
end (* functor Transform *)
