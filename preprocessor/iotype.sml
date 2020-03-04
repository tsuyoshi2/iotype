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
structure Symbol = Symbol ()
structure Field = Field (structure Symbol = Symbol)
structure Record = Record (
	val isSorted = false
	structure Field = Field
)
structure SortedRecord = Record (
	val isSorted = true
	structure Field = Field
)
structure Tyvar = Tyvar ()
structure Ast = Ast (
	structure Record = Record
	structure SortedRecord = SortedRecord
	structure Symbol = Symbol
	structure Tyvar = Tyvar
)
structure FrontEnd = FrontEnd (structure Ast = Ast)
structure Write = Write (structure Ast = Ast)
structure Transform = Transform (structure Ast = Ast)
fun go (inputName, outputName, output) =
	Write.writeProgram (
		inputName
		, outputName
		, output
		, Transform.transformProgram (FrontEnd.lexAndParseFile inputName)
	)
datatype commandLineOption = Output of string | Input of string | UseInput
val optionsWithoutHelp = [
	{
		short = "o"
		, long = ["output"]
		, desc = GetOpt.ReqArg (Output, "file")
		, help =
			"specify output filename (- for stdout) for"
			^ " following input files"
	}, {
		short = "i"
		, long = ["use-input-name"]
		, desc = GetOpt.NoArg (fn () => UseInput)
		, help =
			"use input filename base with \".sml\" appended"
			^ " for output filename for following input files"
	}
]
val helpMessage = GetOpt.usageInfo {header = "", options = optionsWithoutHelp} ^ "\n"
val options = {
	short = "h"
	, long = ["help"]
	, desc = GetOpt.NoArg (fn () => (
		TextIO.output (TextIO.stdErr, helpMessage)
		; OS.Process.exit OS.Process.success
	)), help = "display this help message"
} :: optionsWithoutHelp
fun commandLine () =
	GetOpt.getOpt {
		argOrder = GetOpt.ReturnInOrder Input
		, errFn = fn x => (
			TextIO.output (
				TextIO.stdErr
				, "error: " ^ x ^ "\n"
			); OS.Process.exit OS.Process.failure
		), options = options
	} (CommandLine.arguments ())
fun outputNameFromInputName name =
	let
		val {base, ext} = OS.Path.splitBaseExt name
	in
		OS.Path.joinBaseExt {
			base = if ext = SOME "sml" then name else base
			, ext = SOME "sml"
		}
	end
fun whatToDo () =
	let
		val (options, _) = commandLine ()
		fun loop (collected, collecting, uncollected) = case uncollected of
			nil => (case collecting of
				NONE => rev collected
				| SOME x => rev (x :: collected)
			) | Output name :: next => loop (
				case collecting of NONE => collected | SOME x => x :: collected
				, SOME {output = name, inputs = nil}
				, next
			) | UseInput :: next => loop (
				case collecting of NONE => collected | SOME x => x :: collected
				, NONE
				, next
			) | Input name :: next => (case collecting of
				NONE => loop (
					{output = outputNameFromInputName name, inputs = [name]}
						:: collected
					, NONE
					, next
				) | SOME {output, inputs} => loop (
					collected
					, SOME {output = output, inputs = name :: inputs}
					, next
				)
			)
	in
		loop (nil, NONE, options)
	end
val () = case whatToDo () of
	nil => TextIO.output (TextIO.stdErr, "nothing to do\n")
	| toDo => app (fn {output, inputs} =>
		let
			val (outputName, outstream) =
				if output = "-" then ("<stdout>", TextIO.stdOut)
				else (output, TextIO.openOut output)
		in
			app (fn inputName => go (inputName, outputName, outstream)) inputs
		end
	) toDo
