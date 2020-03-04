(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstAtoms (S: AST_ATOMS_STRUCTS): AST_ATOMS = 
struct

open S

structure Wrap = Region.Wrap
structure AdmitsEquality = AdmitsEquality ()
structure Const = AstConst ()

structure Field = Record.Field

structure Tycon =
   struct
      structure Id = AstId (structure Symbol = Symbol)
      open Id
   end

structure Var = AstId (structure Symbol = Symbol)

structure Con =
   struct
      structure Id = AstId (structure Symbol = Symbol)
      open Id

      val it = fromSymbol (Symbol.itt, Region.bogus)
   end

structure Basid = AstId (structure Symbol = Symbol)
structure Sigid = AstId (structure Symbol = Symbol)
structure Strid = AstId (structure Symbol = Symbol)
structure Fctid = AstId (structure Symbol = Symbol)

structure Vid =
   struct
      structure I = AstId (structure Symbol = Symbol)
      open I

      fun fromCon c = fromSymbol (Con.toSymbol c, Con.region c)
      fun fromVar x = fromSymbol (Var.toSymbol x, Var.region x)
      local
         fun make f v = f (toSymbol v, region v)
      in
         val toCon = make Con.fromSymbol
         val toVar = make Var.fromSymbol
      end
   end

structure Longtycon = Longid (structure Id = Tycon
                            structure Strid = Strid
                            structure Symbol = Symbol)

structure Longvar = Longid (structure Id = Var
                            structure Strid = Strid
                            structure Symbol = Symbol)

structure Longcon =
   struct
      structure L = Longid (structure Id = Con
                            structure Strid = Strid
                            structure Symbol = Symbol)

      open L
   end

structure Longstrid = Longid (structure Id = Strid
                              structure Strid = Strid
                              structure Symbol = Symbol)


structure Longvid =
   struct
      structure L = Longid (structure Id = Vid
                            structure Strid = Strid
                            structure Symbol = Symbol)

      open L
      local
         fun to (make,node, conv) x =
            let val (T {strids, id}, region) = dest x
            in make (node {strids = strids, id =  conv id}, region)
            end
      in
         val toLongcon = to (Longcon.makeRegion, Longcon.T, Vid.toCon)
      end
   end

open Layout

structure Type =
   struct
      structure Record = SortedRecord
      open Wrap
      datatype node =
         Arrow of t * t
       | Con of Longtycon.t * t vector
       | Group of t
       | Record of t Record.t
       | Var of Tyvar.t
      withtype t = node Wrap.t
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val var = make o Var
      val record = make o Record
      val tuple = record o Record.tuple
      val unit = tuple (Vector.fromList nil)

      fun con (c: Tycon.t, ts: t vector): t =
         make (Con (Longtycon.short c, ts))

      fun arrow (t1, t2) = make (Arrow (t1, t2))

      fun layoutApp (tycon, args: 'a vector, layoutArg) =
         case Vector.length args of
            0 => tycon
          | 1 => seq [layoutArg (Vector.sub (args, 0)), str " ", tycon]
          | _ => seq [Layout.tuple (Vector.foldr (fn (x, y) => (layoutArg x) :: y) nil args), str " ", tycon]

      fun layout ty =
         case node ty of
            Var v => Tyvar.layout v
	  | Group t => layout t
          | Arrow (t1, t2) => paren (mayAlign
                                     [layout t1,
                                      seq [str "-> ",
                                           layout t2]])
          | Con (c, tys) => layoutApp (Longtycon.layout c, tys, layout)
          | Record r => Record.layout {record = r,
                                       separator = ":", extra = "",
                                       layoutElt = layout,
                                       layoutTuple = layoutTupleTy}
      and layoutTupleTy tys =
         case Vector.length tys of
            0 => str "unit"
          | 1 => layout (Vector.sub (tys, 0))
          | _ => paren (mayAlign (separateLeft (Vector.foldr (fn (x, y) => (layout x) :: y) nil tys,
                                                "* ")))

      fun layoutOption ty =
         case ty of
            NONE => empty
          | SOME ty => seq [str " of ", layout ty]
   end

fun bind (x, y) = mayAlign [seq [x, str " ="], y]

fun 'a layoutAnds (prefix: string,
                   xs: 'a vector, 
                   layoutX: Layout.t * 'a -> Layout.t): Layout.t =
   case Vector.foldr op :: nil xs of
      [] => empty
    | x :: xs => align (layoutX (str (concat [prefix, " "]), x)
                        :: List.map (fn x => layoutX (str "and ", x)) xs)

datatype bindStyle = OneLine | Split of int

fun 'a layoutBind (bind: string,
                   layout: 'a -> bindStyle * Layout.t * Layout.t)
   (prefix: Layout.t, x: 'a): Layout.t =
   let
      val (style, lhs, rhs) = layout x
      val lhs = seq [prefix, lhs, str " " , str bind]
   in
      case style of
         OneLine => seq [lhs, str " ", rhs]
       | Split indentation => align [lhs, indent (rhs, indentation)]
   end

fun layoutAndsBind (prefix, bind, xs, layout) =
   layoutAnds (prefix, xs, layoutBind (bind, layout))

structure TypBind =
   struct
      datatype node =
         T of {tycon: Tycon.t,
               def: Type.t,
               tyvars: Tyvar.t vector} vector
      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout t =
         let
            val T ds = node t
         in
            layoutAndsBind
            ("type", "=", ds, fn {tycon, def, tyvars} =>
             (OneLine,
              Type.layoutApp (Tycon.layout tycon,
                              tyvars,
                              Tyvar.layout),
              Type.layout def))
         end

      val empty = makeRegion (T (Vector.fromList nil), Region.bogus)

   end

(*---------------------------------------------------*)
(*                      DatBind                      *)
(*---------------------------------------------------*)

structure DatBind =
   struct
      datatype node =
         T of {datatypes: {cons: (Con.t * Type.t option) vector,
                           tycon: Tycon.t,
                           tyvars: Tyvar.t vector} vector,
               withtypes: TypBind.t}

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout (prefix, d) =
         let
            val T {datatypes, withtypes} = node d
         in
            align
            [layoutAndsBind
             (prefix, "=", datatypes, fn {tyvars, tycon, cons} =>
              (OneLine,
               Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout),
               alignPrefix (Vector.foldr (fn ((c, to), l) =>
                                              (seq [Con.layout c,
                                                   Type.layoutOption to]) :: l)
                                         nil
                                         cons,
                           "| "))),
             case TypBind.node withtypes of
                TypBind.T v =>
                   if 0 = Vector.length v
                      then empty
                   else seq [str "with", TypBind.layout withtypes]]
         end

   end

structure DatatypeRhs =
   struct
      datatype node =
         DatBind of DatBind.t
       | Repl of {lhs: Tycon.t, rhs: Longtycon.t}

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout d =
         case node d of
            DatBind d => DatBind.layout ("datatype", d)
          | Repl {lhs, rhs} =>
               seq [str "datatype ", Tycon.layout lhs,
                   str " = datatype ", Longtycon.layout rhs]

   end

(*---------------------------------------------------*)
(*                      ModIdBind                    *)
(*---------------------------------------------------*)

structure ModIdBind =
   struct
      datatype node =
         Fct of {lhs: Fctid.t, rhs: Fctid.t} vector
       | Sig of {lhs: Sigid.t, rhs: Sigid.t} vector
       | Str of {lhs: Strid.t, rhs: Strid.t} vector

      open Wrap
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout d =
         let
            fun doit (prefix, l, bds) =
               layoutAndsBind
               (prefix, "=", bds, fn {lhs, rhs} => (OneLine, l lhs, l rhs))
         in
            case node d of
               Fct bds => doit ("functor", Fctid.layout, bds)
             | Sig bds => doit ("signature", Sigid.layout, bds)
             | Str bds => doit ("structure", Strid.layout, bds)
         end

   end

end
