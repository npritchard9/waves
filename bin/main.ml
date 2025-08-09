open Waves

(* let () = Read_wav.parse_wav "inputs/thermo.wav" *)
let () =
  Read_wav.parse_wav "inputs/thermo.wav"
  |> Read_wav.Wav_header.show |> print_endline
