open Waves

let () =
  Read_wav.parse_wav "inputs/thermo.wav" |> Read_wav.Wav.show |> print_endline
