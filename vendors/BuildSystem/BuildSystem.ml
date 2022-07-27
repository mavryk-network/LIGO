module Errors = Errors

module List = Simple_utils.List
module Export_graph = Graph.Persistent.Digraph.Concrete(struct include String let hash = Hashtbl.hash end)
module SMap = Simple_utils.Map.Make(String)
module SSet = Set.Make(String)

module type M =
  sig
    module File_name : sig
      type raw
      type t [@@deriving eq, compare, to_yojson]
      val pp : Format.formatter -> t -> unit
      val of_file : string -> t
      val of_raw  : raw -> t
      val to_string : t -> string
    end
    type module_name = string
    type compilation_unit
    type meta_data
    val preprocess : File_name.t -> compilation_unit * meta_data * (File_name.t * module_name) list
    module AST : sig
      type declaration
      type t = declaration list
      (* Environment should be a local notion of the BuildSystem *)
      type environment
      val add_module_to_env : module_name -> environment -> environment -> environment
      val add_ast_to_env : t -> environment -> environment
      val init_env : environment

      (* This should probably be taken in charge be the compiler, which should be able to handle "libraries" *)
      val make_module_declaration : module_name -> t -> declaration
    end
    val compile : AST.environment -> File_name.t -> meta_data -> compilation_unit -> AST.t
  end

module Make (M : M) =
  struct
  module File_name = M.File_name
  module Node = struct
    type t = (
      File_name.t
    )
      [@@deriving eq, compare]
    let hash = Hashtbl.hash
  end

  module G = Graph.Persistent.Digraph.Concrete(Node)
  module Dfs = Graph.Traverse.Dfs(G)
  module TopSort = Graph.Topological.Make(G)
  module FMap = Simple_utils.Map.Make(M.File_name)
  module FSet = Set.Make(M.File_name)

  type vertice = M.module_name * M.meta_data * M.compilation_unit * (File_name.t * M.module_name) list
  type graph = G.t * vertice FMap.t
  type error = Errors.t
  type ast = M.AST.t
  type env = M.AST.environment
  type 'a build_error = ('a, error) result

  module PP = struct
    type state = {
      pad_path : string;
      pad_node : string;
    }

    let mk_state () = {
      pad_path = "";
      pad_node = "";
    }

    let pad arity rank {pad_path=_;pad_node} =
      { pad_path =
            pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
          pad_node =
            pad_node ^ (if rank = arity-1 then "    " else "|   ")
      }

    let graph' f (dep_g,node) =
      let exception Dependency_cycle of Node.t in
      let len node =
        let aux _node i = i + 1 in
        G.fold_succ aux dep_g node 0
      in
      let state = mk_state () in
      let set = FSet.empty in
      let rec pp_node state set arity node rank =
        let state = pad arity rank @@ state in
        f state.pad_path @@ node;
        if FSet.mem node set then raise (Dependency_cycle node);
        let set = FSet.add node set in
        let len = len node in
        let _ = G.fold_succ (pp_node state set len) dep_g node 0 in
        rank+1
      in
      let _ = try pp_node state set 1 node 0
        with Dependency_cycle _ -> 0 in
      ()

    let graph ppf (dep_g,node) =
      let module TopSort = Graph.Topological.Make(G) in
      let order,final = TopSort.fold
        (fun node (m,order) -> FMap.add node order m, order + 1)
        dep_g (FMap.empty, 1) in
      graph' (fun pad_path node ->
        match FMap.find_opt node order with
          Some n -> Format.fprintf ppf "%s%d -- %a\n%!" pad_path (final - n) M.File_name.pp node
        | None -> ())
        (dep_g,node)
end
  let dependency_graph : File_name.t -> graph =
    fun file_name ->
    let rec dfs (acc:File_name.t) (dep_g,vertices) (file_name,mangled_name) =
      if not @@ FMap.mem file_name vertices then
        let c_unit, meta_data, deps = M.preprocess file_name in
        let vertices = FMap.add file_name (mangled_name,meta_data,c_unit,deps) vertices in
        let dep_g = G.add_vertex dep_g file_name in
        let dep_g =
          (* Don't add a loop on the first element *)
          if M.File_name.equal acc file_name then dep_g
          else G.add_edge dep_g acc file_name
        in
        let dep_g,vertices =
          List.fold ~f:(dfs file_name) ~init:(dep_g,vertices) deps
        in
        (dep_g,vertices)
      else
        let dep_g = G.add_edge dep_g acc file_name in
        (dep_g,vertices)
    in
    let vertices = FMap.empty in
    let dep_g = G.empty in
    dfs file_name (dep_g,vertices) @@ (file_name,"root")

  let solve_graph : graph -> File_name.t -> ((File_name.t * vertice) list,error) result =
    fun (dep_g,vertices) file_name ->
    if Dfs.has_cycle dep_g
    then (
      let graph = Format.asprintf "%a" PP.graph (dep_g,file_name) in
      Error (Errors.build_dependency_cycle graph)
    )
    else
      let aux v order =
        let elem = FMap.find v vertices in
        (v,elem)::order
      in
      let order = TopSort.fold aux dep_g [] in
      Ok (order)

  let convert_graph : graph -> Export_graph.t * (M.module_name * M.meta_data * M.compilation_unit * (string * M.module_name) list) SMap.t = fun (dep_g,vertices) ->
    let vertices = FMap.to_kv_list vertices
     |> List.map ~f:(fun (k,v) -> File_name.to_string k,v)
     |> SMap.of_list
    in
    let vertices = SMap.map
    (fun (m,d,c,lst) ->
      let lst =
        List.map ~f:(fun (a,b) -> File_name.to_string a,b) lst in
      (m,d,c,lst)
    ) vertices in
    let ext = Export_graph.empty in
    let ext = G.fold_vertex (fun f graph ->
      Export_graph.add_vertex graph (File_name.to_string f)) dep_g ext
    in
    (ext,vertices)

  let aggregate_dependencies_as_headers order_deps asts_typed =
    (* Add the module at the beginning of the file *)
    let aux map ((file_name),(_,_,_,deps_lst)) =
      let (ast,_) =
        match (FMap.find_opt file_name asts_typed) with
          Some ast -> ast
        | None -> failwith "failed to find module"
      in

      let map = FMap.add file_name ast map in
      map
    in
    let asts_typed = List.fold ~f:aux ~init:FMap.empty order_deps in
    (* Separate the program and the dependency (those are process differently) *)
    let (file_name,(_,_,_,_deps_lst)),order_deps = match List.rev order_deps with
      | [] -> failwith "compiling nothing"
      | hd::tl -> (hd,tl) in
    let contract =
      match (FMap.find_opt file_name asts_typed) with
        Some ast -> ast
      | None -> failwith "failed to find module"
    in
    (* Add all dependency at the beginning of the file *)
    let add_modules dep_types (file_name,(mangled_name,_,_, _deps_lst)) =
      let module_binder = mangled_name in
      (* Get the ast_type of the module *)
      let ast_typed =
        match (FMap.find_opt file_name asts_typed) with
          Some ast -> ast
        | None -> failwith "failed to find module"
      in
      (dep_types,(M.AST.make_module_declaration module_binder ast_typed))
    in
    let _,header_list = List.fold_map_right ~f:add_modules ~init:(FMap.empty) @@ order_deps in
    let aggregated = List.fold_left ~f:(fun c a ->  a::c) ~init:contract header_list in
    aggregated

  let add_modules_in_env (env : M.AST.environment) deps =
    let aux env (module_name, (_,ast)) =
      M.AST.add_module_to_env module_name ast env
    in
    List.fold_left ~f:aux ~init:env deps

  let add_deps_to_env (asts_typed : (ast * env) FMap.t) (_file_name, (_meta,_c_unit,deps)) =
    let aux (file_name,module_name) =
      let ast_typed =
        match (FMap.find_opt file_name asts_typed) with
          Some (ast) -> ast
        | None -> failwith "File typed before dependency. The build system is broken, contact the devs"
      in
      (module_name, ast_typed)
    in
    let deps = List.map ~f:aux deps in
    let env_with_deps = add_modules_in_env M.AST.init_env deps in
    env_with_deps

  let compile_file_with_deps asts (file_name, (mangled_name,meta,c_unit,deps)) =
    let env_with_deps = add_deps_to_env asts (file_name, (meta,c_unit,deps)) in
    let ast = M.compile env_with_deps file_name meta c_unit in
    let ast_env = M.AST.add_ast_to_env ast env_with_deps in
    FMap.add file_name (ast,ast_env) asts

  let compile_separate : File_name.t -> ast build_error =
    fun file_name ->
      let deps = dependency_graph file_name in
      match solve_graph deps file_name with
        Ok (ordered_deps) ->
        let asts_typed = List.fold ~f:(compile_file_with_deps) ~init:(FMap.empty) ordered_deps in
        Ok (fst @@ FMap.find file_name asts_typed)
      | Error e -> Error e

  let compile_combined : File_name.t -> ast build_error =
    fun file_name ->
      let deps = dependency_graph file_name in
      match solve_graph deps file_name with
        Ok (ordered_deps) ->
        let asts_typed = List.fold ~f:(compile_file_with_deps) ~init:(FMap.empty) ordered_deps in
        let contract = aggregate_dependencies_as_headers ordered_deps asts_typed in
        Ok(contract)
      | Error e -> Error e

end
module PP = struct
type state = {
  pad_path : string;
  pad_node : string;
}

let mk_state () = {
  pad_path = "";
  pad_node = "";
}

let pad arity rank {pad_path=_;pad_node} =
  { pad_path =
        pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
      pad_node =
        pad_node ^ (if rank = arity-1 then "    " else "|   ")
  }

let graph' f (dep_g,node) =
  let exception Dependency_cycle of String.t in
  let len node =
    let aux _node i = i + 1 in
    Export_graph.fold_succ aux dep_g node 0
  in
  let state = mk_state () in
  let set = SSet.empty in
  let rec pp_node state set arity node rank =
    let state = pad arity rank @@ state in
    f state.pad_path @@ node;
    if SSet.mem node set then raise (Dependency_cycle node);
    let set = SSet.add node set in
    let len = len node in
    let _ = Export_graph.fold_succ (pp_node state set len) dep_g node 0 in
    rank+1
  in
  let _ = try pp_node state set 1 node 0
    with Dependency_cycle _ -> 0 in
  ()

let graph ppf (dep_g,node) =
  let module TopSort = Graph.Topological.Make(Export_graph) in
  let order,final = TopSort.fold
    (fun node (m,order) -> SMap.add node order m, order + 1)
    dep_g (SMap.empty, 1) in
  graph' (fun pad_path node ->
    match SMap.find_opt node order with
      Some n -> Format.fprintf ppf "%s%d -- %s\n%!" pad_path (final - n) node
    | None -> ())
    (dep_g,node)
end
module To_yojson = struct
let graph (dep_g,filename) =
  let set = SSet.empty in
  let rec pp_node set name parent =
    let node = ("file", `String name) in
    if SSet.mem name set then ("child", `Assoc [node])::parent
    else
      let set = SSet.add name set in
      let node = Export_graph.fold_succ (pp_node set) dep_g name [node] in
      let node = List.rev node in
      ("child", `Assoc node)::parent
  in
  let root = ("root", `String filename) in
  let root =
    Export_graph.fold_succ (pp_node set) dep_g filename [root]
  in `Assoc (List.rev root)
end
module Formatter = struct
  open Simple_utils.Display

  let graph_ppformat ~display_format f g =
    match display_format with
    | Human_readable | Dev -> PP.graph f g

  let graph_jsonformat g : json =
    To_yojson.graph g

  let graph_format : 'a format = {
    pp = graph_ppformat;
    to_json = graph_jsonformat;
  }
end
