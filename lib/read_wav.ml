open Core

module Wav_header = struct
  type t = {
    num_channels : int;
    sample_rate : int;
    bits_per_sample : int;
    data_size : int;
  }
  [@@deriving show]
end

open Wav_header

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

      { num_channels; sample_rate; bits_per_sample; data_size })
