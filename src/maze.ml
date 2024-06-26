open! Core

module Pixel = struct
  type t =
    { c : char
    ; coordinates : int * int
    }
  [@@deriving hash, equal, compare, sexp]
end

module G = Graph.Imperative.Graph.Concrete (Pixel)

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
   module Network = struct
    module Connection = struct
      module T = struct
        type t = Pixel.t * Pixel.t [@@deriving compare, sexp]
      end
  
      (* This funky syntax is necessary to implement sets of [Connection.t]s.
         This is needed to defined our [Network.t] type later. Using this
         [Comparable.Make] functor also gives us immutable maps, which might
         come in handy later. *)
      include Comparable.Make (T)
  
      (* let of_string s =
        match String.split s ~on:',' with
        | [ x; y ] -> Some (Person.of_string x, Person.of_string y)
        | _ -> None *)
      ;;
    end
    type t = Connection.Set.t [@@deriving sexp_of]
    let of_file input_file =
      let connect_neighbors char_grid Pixel = 

      let connections =
        In_channel.read_lines (File_path.to_string input_file) in
        let grid = List.map connections ~f:(String.to_list) in 
        List.iter grid ~f
        (* |> List.concat_map ~f:(fun s ->
          match Connection.of_string s with
          | Some (a, b) ->
            (* Friendships are mutual; a connection between a and b means we
               should also consider the connection between b and a. *)
            [ a, b; b, a ]
          | None ->
            printf
              "ERROR: Could not parse line as connection; dropping. %s\n"
              s;
            [])
      in
      Connection.Set.of_list connections *)
    ;;
  end


let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
