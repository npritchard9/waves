open Core
open Read_wav

let read_pcm_data data_size ic =
  let pcm_bytes = Bytes.create data_size in
  let _ = In_channel.really_input ic ~buf:pcm_bytes ~pos:0 ~len:data_size in
  pcm_bytes

let bytes_to_raw_vals pcm_bytes =
  let len = Bytes.length pcm_bytes in
  Array.init len ~f:(fun i -> Char.to_int (Bytes.get pcm_bytes i))

let normalize_raw_vals pcms =
  Array.map pcms ~f:(fun v -> (Float.of_int @@ (v - 128)) /. 128.0)

let write_to_csv samples =
  Out_channel.with_file "output.csv" ~f:(fun oc ->
      Array.iteri samples ~f:(fun i sample -> fprintf oc "%d,%.6f\n" i sample))

let read_data filename =
  let header = Read_wav.parse_wav filename in
  Wav_header.show header |> printf "headers: %s\n";
  let vals =
    In_channel.with_file filename ~binary:true ~f:(fun ic ->
        read_pcm_data header.data_size ic
        |> bytes_to_raw_vals |> normalize_raw_vals)
  in
  write_to_csv vals
