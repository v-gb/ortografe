let max_size = ref 300_000_000

let file_size file =
  Int.max
    1 (* ensure no possible division by zero *)
    (Int.max
       (Zipc.File.decompressed_size file)
       (Zipc.File.compressed_size file))

let map ?(progress = ignore) src f =
  let zipc = Zipc.of_binary_string src |> Core.Result.ok_or_failwith in
  let entries_to_rewrite =
    Zipc.fold (fun member acc ->
        match Zipc.Member.kind member with
        | Dir -> acc
        | File file ->
           match f ~path:(Zipc.Member.path member) with
           | None -> acc
           | Some rewrite_content -> (member, file, rewrite_content) :: acc)
      zipc []
  in
  let bytes_to_read =
    List.fold_left
      (fun acc (_, file, _) -> acc + file_size file)
      0 entries_to_rewrite
  in
  (* Zipc enforces that the content is no larger than the size in the metadata, so this
     check is robust whether large data is submitted accidentally or is adversarial.
     Zip bombs require no special attention: they are just a way to get a higher
     compression ratio than deflate supports (1000x). *)
  if bytes_to_read > !max_size
  then failwith "files in zip too large";
  let _, new_zipc =
    List.fold_left
      (fun (bytes_read, acc) (member, file, rewrite_content) ->
        progress (bytes_read * 100 / bytes_to_read);
        let new_content =
          Zipc.File.to_binary_string file
          |> Core.Result.ok_or_failwith
          |> rewrite_content ~contents:__
        in
        let new_file =
          match Zipc.File.compression file with
          | Stored -> Zipc.File.stored_of_binary_string new_content |> Core.Result.ok_or_failwith
          | Deflate -> Zipc.File.deflate_of_binary_string new_content |> Core.Result.ok_or_failwith
          | _ -> failwith "unknown compression type, should have failed earlier"
        in
        let new_member =
          Zipc.Member.make
            ~mtime:(Zipc.Member.mtime member)
            ~mode:(Zipc.Member.mode member)
            ~path:(Zipc.Member.path member)
            (File new_file)
          |> Core.Result.ok_or_failwith
        in
        (bytes_read + file_size file, Zipc.add new_member acc))
      (0, zipc) entries_to_rewrite
  in
  progress 100;
  Zipc.to_binary_string new_zipc |> Core.Result.ok_or_failwith
