open Netlist_ast
open Graph

exception Combinational_cycle

let read_eq eq = 
  let read_arg arg list = match arg with
    | Avar ident -> ident::list
    | Aconst _ -> list
  in let read_exp exp list = match exp with
    | Earg arg -> read_arg arg list
    | Ereg ident -> ident::list
    | Enot arg -> read_arg arg list
    | Ebinop(_, a1, a2) -> let l = read_arg a1 list in read_arg a2 l
    | Emux(a1, a2, a3) -> 
      let l1 = read_arg a1 list in
      let l2 = read_arg a2 l1 in
      read_arg a3 l2
    | Erom(_, _, arg) -> read_arg arg list
    | Eram(_, _, a1, a2, a3, a4) ->
      let l1 = read_arg a1 list in
      let l2 = read_arg a2 l1 in
      let l3 = read_arg a3 l2 in
      read_arg a4 l3
    | Econcat(a1, a2) -> let l = read_arg a1 list in read_arg a2 l
    | Eslice(_, _, arg) -> read_arg arg list
    | Eselect(_, arg) -> read_arg arg list
  in match eq with
    | (_, exp) -> read_exp exp []

let schedule p =
  let deps_graph = mk_graph () in
  (* Register all equations into the dependecy graph *)
  List.iter (fun eq -> add_node deps_graph eq) p.p_eqs;
  (* Build the dependecy graph *)
  List.iter (fun eq ->
    (* We do not consider references to inputs because they are not important
      in scheduling (for any order, all inputs can be at any time used). *)
    let deps_idents = List.filter (fun ident -> not (List.mem ident p.p_inputs)) (read_eq eq) in
    List.iter (fun ident -> 
      let referenced_eq = List.find (fun (label, _) -> label = ident) p.p_eqs in
      (* referenced_eq "depends on" eq *)
      add_edge deps_graph referenced_eq eq
    ) deps_idents
  ) p.p_eqs;
  (* Return the new program with the equations correctly sorted *)
  { p_eqs = topological deps_graph; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars }
