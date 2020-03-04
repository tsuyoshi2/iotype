(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstPrograms (S: AST_PROGRAMS_STRUCTS): AST_PROGRAMS = 
struct

open S

structure AstModules = AstModules (S)

open AstModules Layout

structure Program =
   struct
      datatype t = T of Topdec.t list list

      val empty = T []

      fun layout (T dss) =
         Layout.align (List.map (fn ds =>
                                 Layout.paren 
                                 (Layout.align (List.map Topdec.layout ds))) dss)

      fun coalesce (T dss): t =
         let
            fun finish (sds, ac) =
               case sds of
                  [] => ac
                | _ =>
                     let
                        val t =
                           Topdec.makeRegion
                           (Topdec.Strdec (Strdec.makeRegion
                                           (Strdec.Seq (rev sds), Region.bogus)),
                            Region.bogus)
                     in
                        t :: ac
                     end
            fun loop (ds, sds, ac) =
               case ds of
                  [] => finish (sds, ac)
                | d :: ds =>
                     case Topdec.node d of
                        Topdec.Strdec d => loop (ds, d :: sds, ac)
                      | _ => loop (ds, [], d :: finish (sds, ac))
         in
            T (List.map (fn ds => rev (loop (ds, [], []))) dss)
         end
   end

end
