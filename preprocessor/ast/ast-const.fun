(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstConst (S: AST_CONST_STRUCTS): AST_CONST =
struct

open S Region.Wrap

datatype node =
   Bool of bool
 | Char of string
 | Int of string
 | Real of string
 | String of string
 | Word of string
type t = node Region.Wrap.t
type node' = node
type obj = t

fun ordToString (c: string): string = c

local
   open Layout
in
   fun layout c =
      case node c of
         Bool b => if b then str "true" else str "false"
       | Char c => str (concat ["#\"", c, "\""])
       | Int s => str s
       | Real l => Layout.str l
       | String s => str (concat ["\"", s, "\""])
       | Word w => str w
end

end
