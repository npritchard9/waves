open Core
open Waves.Wav

let () =
  (* Read_wav.parse_wav "inputs/thermo.wav" |> Read_wav.Wav.show |> print_endline *)
  let wav = parse_wav "inputs/thermo.wav" in
  let min_a, max_a = Wav.min_max_amplitude wav in
  printf "min: %.6f, max: %.6f\n" min_a max_a;
  let rms = Wav.root_mean_square wav in
  printf "rms: %.6f\n" rms
