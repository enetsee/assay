let read (path : string) : string = In_channel.with_open_bin path In_channel.input_all

let lines (path : string) : string list =
  In_channel.with_open_text path In_channel.input_lines
;;
