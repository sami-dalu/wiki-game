open! Core
module Article = String
(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
   module Network = struct
    (* We can represent our social network graph as a set of connections, where
       a connection represents a friendship between two people. *)
    module Connection = struct
      module T = struct
        type t = Article.t * Article.t [@@deriving compare, sexp]
      end
  
      (* This funky syntax is necessary to implement sets of [Connection.t]s.
         This is needed to defined our [Network.t] type later. Using this
         [Comparable.Make] functor also gives us immutable maps, which might
         come in handy later. *)
      include Comparable.Make (T)
    end
  
    (* let rec connect city_list =
      match city_list with
      | head :: tail ->
        List.concat_map tail ~f:(fun city -> [ head, city; city, head ])
        @ connect tail
      | [] -> []
    ;; *)
  
    (* let handle_line csv_line =
      match String.split csv_line ~on:',' with
      | [] -> []
      | _ :: tail -> connect tail
    ;; *)
  
    type t = Connection.Set.t [@@deriving sexp_of]
  
    (* let of_file input_file =
      let connections =
        In_channel.read_lines (File_path.to_string input_file)
        |> List.concat_map ~f:(fun str -> handle_line str)
        (* match Connection.of_string str with | Some (a, b) -> (* Connections
           are mutual; a connection between a and b means we should also
           consider the connection between b and a. *) [ a, b; b, a ] | None ->
           printf "ERROR: Could not parse line as connection; dropping. %s\n"
           s; []) *)
      in
      Connection.Set.of_list connections
    ;; *)
  end

module G = Graph.Imperative.Graph.Concrete (Article)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the
       generated graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
       for examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = sprintf {|"%s"|} v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)
  
(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents 
  $$ "a"
  |> to_list
  |> List.map ~f:(fun node -> attribute "href" node)
  |> List.filter ~f:(fun str -> Option.is_none (Wikipedia_namespace.namespace (Option.value_exn str)))
  |> List.map ~f:(fun stro -> Option.value_exn stro)
  |> List.filter ~f:(fun strz -> String.is_prefix ~prefix:"/wiki" strz)
  |> List.dedup_and_sort ~compare:(String.compare)
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* let to_depth_n_graph (depth : int) ~article : Article.t list =
  let visited = Article.Hash_set.create () in
  let q = Queue.create () in
  Queue.enqueue q (article, 0);
  let rec traverse () =
    match Queue.dequeue q with
    | None -> ()
    | Some node ->
      let linked_articles = get_linked_articles article 
      in
      let linked_articles_tpls = List.map linked_articles ~f:(fun linked_article -> (linked_article, depth+1)) in
      List.iter linked_articles_tpls ~f:(fun neighbor ->
        if not (Hash_set.mem visited (get_article neighbor)) && (get_depth neighbor) < depth then Queue.enqueue q neighbor);
      Hash_set.add visited (get_article node);
      traverse ()
  in
  traverse ();
  Hash_set.to_list visited;; *)

  module Article_info = struct
    type t = {
      name : string;
      depth : int;
    }
  end

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
    let graph = G.create () in
    let visited = Article.Hash_set.create () in
    let q = Queue.create () in
    let origin_info = {Article_info.name = origin; depth = 0} in
    Queue.enqueue q origin_info;
    let rec traverse () =
      let dequeued_node = Queue.dequeue q in
      match dequeued_node with
      | None -> ()
      | Some {name = article_name; depth = article_depth} ->
        File_fetcher.fetch_exn how_to_fetch 
        let linked_articles = get_linked_articles article_name in
        let linked_articles_info = List.map linked_articles ~f:(fun linked_article_name -> {Article_info.name = linked_article_name; depth = article_depth+1}) in
        List.iter linked_articles_info ~f:(fun neighbor_info -> (
          if (neighbor_info.depth) <= max_depth then (
            G.add_edge graph ((neighbor_info.name)) (article_name);
            if not (Hash_set.mem visited (neighbor_info.name)) then 
              (Queue.enqueue q neighbor_info);
        )
        )
        );
        Hash_set.add visited article_name;
        traverse ()
    in
    traverse ();
    (* Hash_set.to_list visited in
        Set.iter network ~f:(fun (city1, city2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          G.add_edge graph city1 city2); *)
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
;;


  
let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Play wiki game by finding a link between the origin and destination pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination = flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
