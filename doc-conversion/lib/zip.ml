let max_size = ref 300_000_000
let count_size () =
  let total_size = ref 0 in
  (fun file ->
    (* Zipc enforces that the content is no larger than the size in the metadata, so this
       check is robust whether large data is submitted accidentally or is adversarial.
       Zip bombs require no special attention: they are just a way to get a higher
       compression ratio than deflate supports (1000x). *)
    total_size := !total_size + Zipc.File.decompressed_size file;
    if !total_size > !max_size
    then failwith "files in zip too large")

let map src f =
  let zipc = Zipc.of_binary_string src |> Core.Result.ok_or_failwith in
  let count = count_size () in
  let new_zipc =
    Zipc.fold (fun member acc ->
        match Zipc.Member.kind member with
        | Dir -> acc
        | File file ->
           match f member (fun () ->
                     count file;
                     Zipc.File.to_binary_string file |> Core.Result.ok_or_failwith) with
           | None -> acc
           | Some new_content ->
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
              Zipc.add new_member acc

      )
      zipc zipc
  in
  Zipc.to_binary_string new_zipc |> Core.Result.ok_or_failwith
