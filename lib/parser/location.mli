type t

type raw_pos = Lexing.position * Lexing.position

val create : raw_pos -> t

val print_location : Format.formatter -> t -> Unit.t

val start_line_num : t -> Int.t

val filename : t -> String.t
