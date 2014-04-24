(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Sqlite3 storage. *)

open Sqlite3

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_sqlite" "RDF_SQLITE_DEBUG_LEVEL"

let name = "sqlite"

type t = {
  db : Sqlite3.db;
  name : Rdf_iri.iri;
}

type error = 
  | Sqlite of Sqlite3.stmt * Sqlite3.Rc.t
  | Sqlite_bind of Sqlite3.stmt * int * Sqlite3.Rc.t

exception Error of error

let tell_error e =
  raise (Error e)

let step ?(expected=Rc.DONE) st =
  let res = step st in
  if res <> expected then tell_error (Sqlite (st, res))

let bind st pos data =
  let res = Sqlite3.bind st pos data in
  if res <> Rc.OK
  then tell_error (Sqlite_bind (st, pos, res))

let string_of_error = function
  | Sqlite(_, res) -> Printf.sprintf "A sqlite error occurred: got
  unexpected %s" (Sqlite3.Rc.to_string res)
  | Sqlite_bind _ -> "A sqlite binding error occurred"

let db_type_iri = Data.TEXT "iri"
let db_type_literal = Data.TEXT "literal"
let db_type_blank = Data.TEXT "blank"

let create_tables db =
  let reqs = [
    "CREATE TABLE IF NOT EXISTS triples (id BLOB, subject TEXT, predicate TEXT, object TEXT)";
    "CREATE TABLE IF NOT EXISTS terms (id BLOB, type TEXT, value TEXT)";
    "CREATE TABLE IF NOT EXISTS literals (id BLOB, type TEXT, value TEXT, lang TEXT)";
    "CREATE TABLE IF NOT EXISTS namespaces (prefix TEXT, uri TEXT)";
  ] in
  List.iter (fun req -> step (prepare db req)) reqs

let sql_of_iri i =
  Data.TEXT (Rdf_iri.string i)

let open_graph ?(options=[]) name =
  let filename = Rdf_graph.get_option "filename" options in
  let g = {
    db = db_open filename;
    name = name;
  } in
  create_tables g.db;
  g

let graph_name g = g.name

let graph_size g =
  let st = prepare g.db "SELECT COUNT(subject) FROM triples" in
  step ~expected:Rc.ROW st;
  match column st 0 with
  | Data.INT i -> Int64.to_int i
  | _ -> assert false

let find_term ?(add=false) g term =
  let sts = prepare g.db "SELECT id FROM terms WHERE type=? AND value=?"
  and stsl = prepare g.db "SELECT id FROM literals WHERE type=? AND value=? AND lang=?"
  and sti = prepare g.db "INSERT INTO terms (id, type, value) VALUES (?, ?, ?)"
  and stl = prepare g.db "INSERT INTO literals (id, type, value, lang) VALUES (?, ?, ?, ?)"
  in
  match term with
  | Rdf_term.Iri iri ->
      let iri_str = sql_of_iri iri in
      bind sts 1 db_type_iri;
      bind sts 2 iri_str;
      if Sqlite3.step sts <> Rc.ROW
      then (
        if not add then raise Not_found
        else (
          let id = Data.BLOB (Uuidm.(to_bytes (create `V4))) in
          bind sti 1 id;
          bind sti 2 db_type_iri;
          bind sti 3 iri_str;
          step sti;
          id)
      )
      else (column sts 0)

  | Rdf_term.Literal lit ->
      let lit_type_str = (match lit.Rdf_term.lit_type with
        | None -> Data.NULL
        | Some i -> sql_of_iri i)
      and lit_lang_str = (match lit.Rdf_term.lit_language with
        | None -> Data.NULL
        | Some l -> Data.TEXT l
      ) in
      bind stsl 1 lit_type_str;
      bind stsl 2 (Data.TEXT lit.Rdf_term.lit_value);
      bind stsl 3 lit_lang_str;
      if Sqlite3.step stsl <> Rc.ROW
      then (
        if not add then raise Not_found
        else (
          let id = Data.BLOB (Uuidm.(to_bytes (create `V4))) in
          bind sti 1 id;
          bind sti 2 db_type_literal;
          bind sti 3 (Data.NULL);
          step sti;
          bind stl 1 id;
          bind stl 2 lit_type_str;
          bind stl 3 (Data.TEXT lit.Rdf_term.lit_value);
          bind stl 4 lit_lang_str;
          step stl;
          id
        )
      )
      else (column stsl 0)

  | Rdf_term.Blank -> Data.NULL

  | Rdf_term.Blank_ blk ->
      let blk_str = Data.TEXT (Rdf_term.string_of_blank_id blk) in
      bind sts 1 db_type_blank;
      bind sts 2 blk_str;
      if Sqlite3.step sts <> Rc.ROW
      then (
        if not add then raise Not_found
        else (
          let id = Data.BLOB (Uuidm.(to_bytes (create `V4))) in
          bind sti 1 id;
          bind sti 2 db_type_blank;
          bind sti 3 blk_str;
          step sti;
          id
        )
      )
      else (column sts 0)

let add_triple g ~sub ~pred ~obj =
  let st = prepare g.db
    "INSERT INTO triples (id, subject, predicate, object) VALUES (?, ?, ?, ?)" in
  bind st 1 (Data.BLOB (Uuidm.(to_bytes (create `V4))));
  bind st 2 (find_term ~add:true g sub);
  bind st 3 (sql_of_iri pred);
  bind st 4 (find_term ~add:true g obj);
  step st

let rem_triple g ~sub ~pred ~obj =
  let st = prepare g.db
    "DELETE FROM triples WHERE subject=?  AND predicate=? AND object=?" in
  try
    bind st 1 (find_term g sub);
    bind st 2 (sql_of_iri pred);
    bind st 3 (find_term g obj);
    step st;
    ()
  with Not_found -> ()

let add_triple_t g (sub, pred, obj) =
  add_triple g ~sub:sub ~pred:pred ~obj:obj

let rem_triple_t g (sub, pred, obj) =
  rem_triple g ~sub:sub ~pred:pred ~obj:obj

let step_map f st =
  let rec aux_step res =
    match Sqlite3.step st with
    | Rc.ROW -> aux_step ((f st) :: res)
    | Rc.DONE -> res
    | res -> tell_error (Sqlite(st, res))
  in aux_step []

let term_out_of_db ?(offset=0) lit_ok st =
  let ty = column st (offset + 0)
  and vl = column st (offset + 1) in
  match ty with
  | Data.TEXT "iri" -> Rdf_term.Iri (Rdf_iri.iri (Data.to_string vl))
  | Data.TEXT "literal" ->
      if not lit_ok
      then raise Not_found (*FIXME proper exception*)
      else Rdf_term.(Literal {
        lit_type = (match column st (offset + 2) with
          | Data.TEXT t -> Some (Rdf_iri.iri t)
          | _ -> None);
        lit_value = Data.to_string (column st (offset + 3));
        lit_language = (match column st (offset + 4) with
          | Data.NULL | Data.NONE -> None
          | Data.TEXT t -> Some t
          | _ -> None);
      })
  | Data.TEXT "blank" ->
      Rdf_term.(Blank_ (blank_id_of_string (Data.to_string vl)))
  | _ -> assert false

let prepare_a_find ?(distinct=false) ?sub ?pred ?obj g =
  let (col_sub, where_sub, bind_sub) =
    match sub with
    | None -> (["sub.type"; "sub.value"], [], [])
    | Some sub -> ([], ["sub.type=:sut"; "sub.value=:suv"], (
        match sub with
        | Rdf_term.Iri i -> [("sut", db_type_iri); ("suv", sql_of_iri i)]
        | Rdf_term.Blank_ i -> [("sut", db_type_blank);
          ("suv", Data.TEXT (Rdf_term.string_of_blank_id i))]
        | _ -> assert false
      )
    )
  in

  let (col_pred, where_pred, bind_pred) =
    match pred with
    | None -> (["triples.predicate"], [], [])
    | Some i -> ([], ["triples.predicate=:prd"], [("prd", sql_of_iri i)])
  in

  let (col_obj, where_obj, bind_obj) =
    match obj with
    | None -> (["obj.type"; "obj.value"; "objl.type"; "objl.value"; "objl.lang"], [], [])
    | Some obj ->
        let bw = ["obj.type=:obt"; "obj.value=:obv"] in
        match obj with 
        | Rdf_term.Iri i -> ([], bw, [("obt", db_type_iri); ("obv", sql_of_iri i)])
        | Rdf_term.Blank_ i -> ([], bw,
          [("obt", db_type_blank); ("obv", Data.TEXT (Rdf_term.string_of_blank_id i))])
        | Rdf_term.Literal l -> ([],
          ["objl.type=:olt"; "objl.value=:olv"; "objl.lang=:oll"] @ bw,
          [
            ("obt", db_type_literal);
            ("obv", Data.NULL);
            ("olt", match l.Rdf_term.lit_type with
              | None -> Data.NULL
              | Some lit_type -> sql_of_iri lit_type
            );
            ("olv", Data.TEXT l.Rdf_term.lit_value);
            ("oll", match l.Rdf_term.lit_language with
              | None -> Data.NULL
              | Some lang -> Data.TEXT lang
            )
          ]
          )
        | Rdf_term.Blank -> assert false
  in

  let query =
    "SELECT " ^ (if distinct then "DISTINCT " else "") ^ (
      match col_sub @ col_pred @ col_obj with
      | [] -> "triples.id"
      | cols -> String.concat ", " cols) ^
    " FROM terms sub JOIN triples t ON sub.id = t.subject JOIN terms obj
    ON obj.id = t.object LEFT JOIN literals objl ON obj.id = objl.id" ^
    (
      match where_sub @ where_pred @ where_obj with
      | [] -> ""
      | wheres -> " WHERE " ^ String.concat " AND " wheres
    ) in

  let st = prepare g.db query in
  (* now, bind *)
  List.iter
    (fun (name, value) -> bind st (bind_parameter_index st name) value; ())
    (bind_sub @ bind_pred @ bind_obj);
  st

let subjects_of g ~pred ~obj =
  let st = prepare_a_find ~pred ~obj g in
  step_map (term_out_of_db false) st

let predicate_out_of_db col st =
  match column st col with
  | Data.TEXT t -> Rdf_iri.iri t
  | _ -> assert false

let predicates_of g ~sub ~obj =
  let st = prepare_a_find ~sub ~obj g in
  step_map (predicate_out_of_db 0) st

let objects_of g ~sub ~pred =
  let st = prepare_a_find ~sub ~pred g in
  step_map (term_out_of_db true) st

let find ?sub ?pred ?obj g =
  let triples_out_of_db st =
    (term_out_of_db ~offset:0 false st,
    predicate_out_of_db 2 st,
    term_out_of_db ~offset:3 true st)
  in 
  let st = prepare_a_find ?sub ?pred ?obj g in
  step_map triples_out_of_db st

let exists ?sub ?pred ?obj g =
  let st = prepare_a_find ?sub ?pred ?obj g in
  (Sqlite3.step st = Rc.ROW)

let exists_t (sub, pred, obj) = exists ~sub ~pred ~obj

let subjects g =
  let st = prepare g.db "SELECT DISTINCT terms.type, terms.value FROM terms,
  triples WHERE terms.id = triples.subject" in
  step_map (term_out_of_db false) st

let objects g =
  let st = prepare g.db "SELECT DISTINCT terms.type, terms.value,
  literals.type, literals.value, literals.lang FROM terms LEFT JOIN
  literals ON terms.id=literals.id, triples WHERE terms.id = triples.subject" in
  step_map (term_out_of_db true) st

let predicates g =
  let st = prepare g.db "SELECT DISTINCT predicate FROM triples" in
  step_map (fun st -> match column st 0 with
  | Data.TEXT i -> Rdf_iri.iri i
  | _ -> assert false) st

let transaction_start g =
  let st = prepare g.db "BEGIN TRANSACTION" in step st

let transaction_commit g =
  let st = prepare g.db "COMMIT TRANSACTION" in step st

let transaction_rollback g =
  let st = prepare g.db "ROLLBACK TRANSACTION" in step st

let new_blank_id g =
  (*FIXME: check that this id does not exist in database *)
  Rdf_term.blank_id_of_string ("genid" ^ (string_of_int (Random.bits ())))

let namespaces g =
  let one_namespace st =
    ((match column st 0 with
    | Data.TEXT s -> Rdf_iri.iri s
    | _ -> assert false)
    ,
    (match column st 1 with
    | Data.TEXT s -> s
    | _ -> assert false)
    )
  in
  let st = prepare g.db "SELECT (prefix, uri) FROM namespaces" in
  step_map one_namespace st

let add_namespace g iri prefix =
  let st = prepare g.db "INSERT INTO namespaces (prefix, uri) VALUES (?,?)" in
  bind st 1 (Data.TEXT prefix);
  bind st 2 (sql_of_iri iri);
  step st

let rem_namespace g prefix =
  let st = prepare g.db "DELETE FROM namespaces WHERE prefix=?" in
  bind st 1 (Data.TEXT prefix);
  step st

let set_namespaces g ns =
  let st = prepare g.db "DELETE FROM namespaces" in
  step st;
  List.iter (fun (iri, prefix) -> add_namespace g iri prefix) ns

module BGP = struct
  type g = t

  type term = (Data.t * Data.t)

  let term g t =
    let sto = prepare g.db "SELECT terms.id FROM terms WHERE type=? AND value=?" in
    let stl = prepare g.db "SELECT terms.id FROM terms LEFT JOIN literals ON
    terms.id=literals.id WHERE terms.type=? AND literals.type=? AND
    literals.value=? AND literals.lang=?" in
    if t = Rdf_term.Blank
    then (Data.NULL, Data.NONE)
    else
      (let st = match t with
      | Rdf_term.Iri i ->
          bind sto 1 db_type_iri;
          bind sto 2 (sql_of_iri i);
          sto
      | Rdf_term.Literal l ->
          bind stl 1 db_type_literal;
          bind stl 2 (match l.Rdf_term.lit_type with
          | None -> Data.NULL
          | Some t -> sql_of_iri t);
          bind stl 3 (Data.TEXT (l.Rdf_term.lit_value));
          bind stl 4 (match l.Rdf_term.lit_language with
          | None -> Data.NULL
          | Some l -> Data.TEXT l);
          stl
      | Rdf_term.Blank_ b ->
          bind sto 1 db_type_blank;
          bind sto 2 (Data.TEXT (Rdf_term.string_of_blank_id b));
          sto
      | Rdf_term.Blank -> assert false

      in
      (
        (try step ~expected:Rc.ROW st; column st 0
        with Error (Sqlite _) -> Data.NONE),
        (match t with
        | Rdf_term.Iri i -> sql_of_iri i
        | _ -> Data.NONE)
      )
      )

  let compare g t1 t2 = Pervasives.compare t1 t2

  let rdfterm g = function
    | (Data.NULL, _) -> Rdf_term.Blank
    | (Data.BLOB _ as t, _) ->
        let st = prepare g.db "SELECT terms.type, terms.value,
        literals.type, literals.value, literals.lang FROM terms LEFT JOIN
        literals ON terms.id=literals.id WHERE terms.id=?" in
        bind st 1 t;
        step ~expected:Rc.ROW st;
        term_out_of_db true st
    | (Data.NONE, t) | (t, _) -> Rdf_term.Iri (Rdf_iri.iri (Data.to_string t))

  let col0 st = (column st 0, Data.NONE)

  let subjects g =
    let st = prepare g.db "SELECT subject FROM triples" in
    step_map col0 st

  let objects g =
    let st = prepare g.db "SELECT object FROM triples" in
    step_map col0 st

  let find ?sub ?pred ?obj g =
    let base_query = "SELECT subject, predicate, object FROM triples" in

    let conditions, values = List.fold_right
    (fun
      (name, proj, value)
      (res_cond, res_val)
      ->
      match value with
      | None -> (res_cond, res_val)
      | Some value -> (((name ^ "=?") :: res_cond), (proj value :: res_val))
    )
    [("subject", fst, sub); ("predicate", snd, pred); ("object", fst, obj)]
    ([], [])
    in

    let query = (
      if conditions <> []
      then base_query ^ " WHERE " ^ (String.concat " AND " conditions)
      else base_query
    ) in
    let st = prepare g.db query in
    List.iteri (fun i bd -> bind st (i+1) bd; ()) values;
    let res = step_map (fun st -> (
      (column st 0, Data.NONE),
      (Data.NONE, column st 1),
      (column st 2, Data.NONE))) st
    in
    res
end

module Sqlite = struct
  let name = name
  type g = t
  (* Type declarations are always implicitely recursive. Use a dummy
   * intermediate type to break the cycle.
   * <https://blogs.janestreet.com/ocaml-annoyance-23-type-declarations-are-implicitly-recursive/>
   *)
  type error' = error
  type error = error'
  exception Error = Error
  let string_of_error = string_of_error
  let open_graph = open_graph
  let graph_name = graph_name
  let graph_size = graph_size
  let add_triple = add_triple
  let rem_triple = rem_triple
  let add_triple_t = add_triple_t
  let rem_triple_t = rem_triple_t
  let subjects_of = subjects_of
  let predicates_of = predicates_of
  let objects_of = objects_of
  let find = find
  let exists = exists
  let exists_t = exists_t
  let subjects = subjects
  let predicates = predicates
  let objects = objects
  let transaction_start = transaction_start
  let transaction_commit = transaction_commit
  let transaction_rollback = transaction_rollback
  let new_blank_id = new_blank_id
  let namespaces = namespaces
  let add_namespace = add_namespace
  let rem_namespace = rem_namespace
  let set_namespaces = set_namespaces
  module BGP = BGP
end

let _ = Rdf_graph.add_storage (module Sqlite : Rdf_graph.Storage)
