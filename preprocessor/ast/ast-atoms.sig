(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_ATOMS_STRUCTS = 
   sig
      structure Record: RECORD
      structure SortedRecord: RECORD
      structure Symbol: SYMBOL
      structure Tyvar: TYVAR
      sharing Record.Field = SortedRecord.Field
      sharing Symbol = Record.Field.Symbol
   end

signature AST_ATOMS =
   sig
      include AST_ATOMS_STRUCTS

      structure Const: AST_CONST

      structure Tycon:
         sig
            include AST_ID
         end

      structure Var: AST_ID

      structure Con:
         sig
            include AST_ID

         end

      structure Basid: AST_ID
      structure Sigid: AST_ID
      structure Strid: AST_ID
      structure Fctid: AST_ID

      structure Vid:
         sig
            include AST_ID

            (* conversions to and from variables and constructors *)
            val fromVar: Var.t -> t
            val fromCon: Con.t -> t
            val toVar: t -> Var.t
            val toCon: t -> Con.t
         end

      structure Longtycon:
         sig
            include LONGID
         end sharing Longtycon.Id = Tycon

      structure Longvar: LONGID sharing Longvar.Id = Var
      structure Longcon: LONGID sharing Longcon.Id = Con
      structure Longstrid: LONGID sharing Longstrid.Id = Strid
      structure Longvid:
         sig
            include LONGID

            val toLongcon: t -> Longcon.t
         end sharing Longvid.Id = Vid

      sharing Strid = Longtycon.Strid = Longvar.Strid = Longcon.Strid
         = Longvid.Strid = Longstrid.Strid

      sharing Symbol = Basid.Symbol = Con.Symbol = Fctid.Symbol = Longcon.Symbol
         = Longstrid.Symbol = Longtycon.Symbol = Longvar.Symbol = Longvid.Symbol
         = Sigid.Symbol = Strid.Symbol = Tycon.Symbol = Vid.Symbol = Var.Symbol

      structure Type:
         sig
            type t
            datatype node =
               Arrow of t * t
             | Con of Longtycon.t * t vector
	     | Group of t
             | Record of t SortedRecord.t
             | Var of Tyvar.t

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val arrow: t * t -> t
            val con: Tycon.t * t vector -> t
            val layout: t -> Layout.t
            val layoutApp: Layout.t * 'a vector * ('a -> Layout.t) -> Layout.t
            val layoutOption: t option -> Layout.t
            val record: t SortedRecord.t -> t
            val tuple: t vector -> t
            val unit: t
            val var: Tyvar.t -> t
         end
      structure TypBind:
         sig
            type t
            datatype node = T of {def: Type.t,
                                  tycon: Tycon.t,
                                  tyvars: Tyvar.t vector} vector
            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val empty: t
            val layout: t -> Layout.t
         end
      structure DatBind:
         sig
            type t
            datatype node =
               T of {datatypes: {cons: (Con.t * Type.t option) vector,
                                 tycon: Tycon.t,
                                 tyvars: Tyvar.t vector} vector,
                     withtypes: TypBind.t}
            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val layout: string * t -> Layout.t
         end
      structure DatatypeRhs:
         sig
            type t
            datatype node =
               DatBind of DatBind.t
             | Repl of {lhs: Tycon.t, rhs: Longtycon.t}
            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val layout: t -> Layout.t
         end
      structure ModIdBind:
         sig
            type t
            datatype node = 
               Fct of {lhs: Fctid.t, rhs: Fctid.t} vector
             | Sig of {lhs: Sigid.t, rhs: Sigid.t} vector
             | Str of {lhs: Strid.t, rhs: Strid.t} vector
            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val layout: t -> Layout.t
         end

      val bind: Layout.t * Layout.t -> Layout.t
      val layoutAnds: (string * 'a vector * (Layout.t * 'a -> Layout.t)
                       -> Layout.t)
      datatype bindStyle =
         OneLine
       | Split of int
      val layoutAndsBind:
         string * string * 'a vector * ('a -> bindStyle * Layout.t * Layout.t)
         -> Layout.t
   end
