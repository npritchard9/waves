open Core

module Wav = struct
  type t = {
    num_channels : int;
    sample_rate : int;
    bits_per_sample : int;
    data_size : int;
    samples : float array;
  }
  [@@deriving show]

  let min_max_amplitude wav =
    Array.fold wav.samples ~init:(Float.infinity, Float.neg_infinity)
      ~f:(fun (min_a, max_a) sample ->
        (Float.min min_a sample, Float.max max_a sample))

  let root_mean_square wav =
    let sum_sq =
      Array.fold wav.samples ~init:0. ~f:(fun acc x -> acc +. (x *. x))
    in
    sqrt (sum_sq /. (Float.of_int @@ Array.length wav.samples))
end

open Wav

let read_u16_le ic =
  let bytes = Bytes.create_local 2 in
  let _ = In_channel.really_input ic ~buf:bytes ~pos:0 ~len:2 in
  Char.to_int (Bytes.get bytes 0) lor (Char.to_int (Bytes.get bytes 1) lsl 8)

let read_u32_le ic =
  let bytes = Bytes.create_local 4 in
  let _ = In_channel.really_input ic ~buf:bytes ~pos:0 ~len:4 in
  Char.to_int (Bytes.get bytes 0)
  lor (Char.to_int (Bytes.get bytes 1) lsl 8)
  lor (Char.to_int (Bytes.get bytes 2) lsl 16)
  lor (Char.to_int (Bytes.get bytes 3) lsl 24)

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

let parse_wav filename =
  In_channel.with_file filename ~binary:true ~f:(fun ic ->
      let bytes = Bytes.create_local 4 in
      let _ = In_channel.really_input ic ~buf:bytes ~pos:0 ~len:4 in
      let riff = Bytes.to_string bytes in
      print_endline riff;
      if not @@ String.equal riff "RIFF" then failwith "Not a RIFF file";
      read_u32_le ic |> printf "file size: %d\n";
      let _ = In_channel.really_input ic ~buf:bytes ~pos:0 ~len:4 in
      let wave = Bytes.to_string bytes in
      print_endline wave;
      if not @@ String.equal wave "WAVE" then failwith "Not a WAVE file";

      (* fmt chunk *)
      let _ = In_channel.really_input ic ~buf:bytes ~pos:0 ~len:4 in
      let fmt_id = Bytes.to_string bytes in
      print_endline fmt_id;
      if not @@ String.equal fmt_id "fmt " then failwith "Missing fmt chunk";
      let _fmt_size = read_u32_le ic in
      let audio_format = read_u16_le ic in
      printf "%d\n" audio_format;
      if audio_format <> 1 then failwith "Not PCM";
      let num_channels = read_u16_le ic in
      let sample_rate = read_u32_le ic in
      read_u32_le ic |> printf "byte rate: %d\n";
      read_u16_le ic |> printf "block align: %d\n";
      let bits_per_sample = read_u16_le ic in

      (* find data chunk *)
      let rec find_data ic =
        let _ = In_channel.really_input ic ~buf:bytes ~pos:0 ~len:4 in
        let chunk_id = Bytes.to_string bytes in
        let chunk_size = read_u32_le ic in
        match chunk_id with
        | "data" -> chunk_size
        | _ ->
            let pos = In_channel.pos ic in
            let next_pos = Int64.(pos + Int64.of_int chunk_size) in
            let next_pos =
              if chunk_size land 1 = 1 then Int64.(next_pos + 1L) else next_pos
            in
            In_channel.seek ic next_pos;
            find_data ic
      in
      let data_size = find_data ic in
      let samples =
        read_pcm_data data_size ic |> bytes_to_raw_vals |> normalize_raw_vals
      in
      (* write_to_csv samples; *)

      { num_channels; sample_rate; bits_per_sample; data_size; samples })
