open! Core

module Pixel = struct
  module PixelT = struct
    type t = { coordinates : int * int }
    [@@deriving hash, equal, compare, sexp]
  end

  include Hashable.Make (PixelT)
  include PixelT
end

module G = Graph.Imperative.Graph.Concrete (Pixel)

module Board = struct
  type t =
    { mutable start_pixel : Pixel.t option [@hash.ignore]
    ; mutable end_pixel : Pixel.t option [@hash.ignore]
    }
  [@@deriving hash, equal, compare, sexp]
end

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
(* module Network = struct module Connection = struct module T = struct type
   t = Pixel.t * Pixel.t [@@deriving compare, sexp] end

   (* This funky syntax is necessary to implement sets of [Connection.t]s.
   This is needed to defined our [Network.t] type later. Using this
   [Comparable.Make] functor also gives us immutable maps, which might come
   in handy later. *) include Comparable.Make (T)

   (* let of_string s = match String.split s ~on:',' with | [ x; y ] -> Some
   (Person.of_string x, Person.of_string y) | _ -> None *) end

   type t = Connection.Set.t [@@deriving sexp_of] *)

let of_file input_file =
  let graph = G.create () in
  let connect graph_ v1 v2 =
    G.add_edge graph_ v1 v2;
    G.add_edge graph_ v2 v1
  in
  let endpoints = { Board.start_pixel = None; end_pixel = None } in
  let connections = In_channel.read_lines (File_path.to_string input_file) in
  let grid = List.map connections ~f:String.to_list in
  let pixels_table = Hashtbl.create (module Pixel) in
  List.iteri grid ~f:(fun row line ->
    List.iteri line ~f:(fun col char ->
      if Char.( <> ) char '#'
      then (
        Hashtbl.add_exn
          pixels_table
          ~key:{ Pixel.coordinates = row, col }
          ~data:char;
        if Char.( = ) char 'S'
        then (
          let start = { Pixel.coordinates = row, col } in
          endpoints.start_pixel <- Some start);
        if Char.( = ) char 'E'
        then (
          let start = { Pixel.coordinates = row, col } in
          endpoints.end_pixel <- Some start))));
  Hashtbl.iter_keys pixels_table ~f:(fun { Pixel.coordinates = row, col } ->
    let below = { Pixel.coordinates = row + 1, col } in
    if Option.is_some (Hashtbl.find pixels_table below)
    then connect graph { Pixel.coordinates = row, col } below;
    let above = { Pixel.coordinates = row - 1, col } in
    if Option.is_some (Hashtbl.find pixels_table above)
    then connect graph { Pixel.coordinates = row, col } above;
    let left = { Pixel.coordinates = row, col - 1 } in
    if Option.is_some (Hashtbl.find pixels_table left)
    then connect graph { Pixel.coordinates = row, col } left;
    let right = { Pixel.coordinates = row, col + 1 } in
    if Option.is_some (Hashtbl.find pixels_table left)
    then connect graph { Pixel.coordinates = row, col } right);
  let s_pixel = Option.value_exn endpoints.start_pixel in
  let e_pixel = Option.value_exn endpoints.end_pixel in
  let stk = Stack.create () in
  Stack.push stk s_pixel;
  let visited = Pixel.Hash_set.create () in
  let rec dfs g (origin : Pixel.t) (target : Pixel.t) =
    let neighbors = G.pred g origin in
    let unvisited_neighbors =
      List.filter neighbors ~f:(fun v ->
        not (Hash_set.exists visited ~f:(Pixel.equal v)))
    in
    let q = Queue.of_list unvisited_neighbors in
    Hash_set.add visited origin;
    let rec traverse () =
      match Queue.dequeue q with
      | Some neighbor ->
        if Pixel.equal neighbor target
        then [ origin; target ]
        else (
          (* List.iter (dfs g neighbor target) ~f:(fun v -> print_s [%message
             (v : Pixel.t)]); *)
          (* print_string "done printing\n"; *)
          let possible_path = dfs g neighbor target in
          let last_n = List.last possible_path in
          if Option.is_some last_n
             (* && Pixel.equal (Option.value ~default:origin last_n)
                target *)
          then [ origin ] @ possible_path
          else traverse ())
      | _ -> []
    in
    traverse ()
  in
  (* List.iter (dfs graph s_pixel e_pixel) ~f:(fun v -> print_s [%message (v
     : Pixel.t)]); print_string "solving maze!\n"; *)
  dfs graph s_pixel e_pixel
;;

(* let rec traverse target path = List.iter path ~f:(fun n -> print_s
   [%message (n : Pixel.t)]); let popped_node = Stack.pop stk in match
   popped_node with | None -> print_string "done\n"; [] | Some pxl ->
   print_string "done\n"; Hash_set.add visited pxl; if Pixel.equal pxl target
   then ( print_string "found target!"; path @ [ target ]) else ( let
   neighbors = G.pred graph pxl in (* code review: how do i prematurely stop
   iteration in the case that i found a solution? imagine i was doing a
   linear search or smth *) List.iter neighbors ~f:(fun neighbor -> if not
   (Hash_set.mem visited neighbor) then Stack.push stk neighbor); traverse
   pxl (path @ [ pxl ])) in traverse e_pixel [] *)

(* |> List.concat_map ~f:(fun s -> match Connection.of_string s with | Some
   (a, b) -> (* Friendships are mutual; a connection between a and b means we
   should also consider the connection between b and a. *) [ a, b; b, a ] |
   None -> printf "ERROR: Could not parse line as connection; dropping. %s\n"
   s; []) in Connection.Set.of_list connections *)

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
        List.iter (of_file input_file) ~f:(fun v ->
          print_s [%message (v : Pixel.t)])]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
