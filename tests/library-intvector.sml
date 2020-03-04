val () = OS.Process.exit (
	let
		val name = OS.FileSys.tmpName ()
	in
		let
			val outstream = BinIO.openOut name
		in
			writeVector writeInt (outstream, Vector.fromList [1234, 5678, 9012])
			; BinIO.closeOut outstream
		end
		; let
			val instream = BinIO.openIn name
		in
			(case readVector readInt instream of
				SOME vector => (case Vector.foldr op :: nil vector of
					[1234, 5678, 9012] => OS.Process.success
					| _ => OS.Process.failure
				) | _ => OS.Process.failure
			) before BinIO.closeIn instream
		end
		before OS.FileSys.remove name
	end
)
