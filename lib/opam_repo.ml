(* opam-repository loader: parses packages/<n>/<n>.<v>/opam files into a
   parser-independent representation of filtered dependency formulas. *)

type rel = Lt | Le | Eq | Neq | Ge | Gt

let rel_holds rel c =
  match rel with
  | Lt -> c < 0
  | Le -> c <= 0
  | Eq -> c = 0
  | Neq -> c <> 0
  | Ge -> c >= 0
  | Gt -> c > 0

let complement = function Lt -> Ge | Le -> Gt | Eq -> Neq | Neq -> Eq | Ge -> Lt | Gt -> Le

let rel_to_string = function
  | Lt -> "<" | Le -> "<=" | Eq -> "=" | Neq -> "!=" | Ge -> ">=" | Gt -> ">"

(* Filter tree attached to a dependency atom (or the available: field). *)
type filter =
  | FBool of bool
  | FVer of rel * string (* version constraint on the enclosing atom's package *)
  | FVar of string (* bare flag, e.g. with-test, build *)
  | FCmp of string * rel * string (* variable comparison *)
  | FNot of filter
  | FAnd of filter * filter
  | FOr of filter * filter

type dep_formula =
  | DAtom of string * filter list (* package name, attached filters (conjoined) *)
  | DAnd of dep_formula list
  | DOr of dep_formula list

type opkg = {
  name : string;
  version : string;
  depends : dep_formula;
  conflicts : dep_formula; (* disjunctive: conflict if any atom matches *)
  conflict_classes : string list;
  available : filter;
}

(* Parsing via opam-format: OpamFile.OPAM gives typed fields, and
   partial evaluation with a package-local environment resolves the
   pseudo-variables (version, name) by the library's own semantics, leaving
   global variables symbolic for the variable-formula encoding. *)

let rel_of_relop : OpamTypes.relop -> rel = function
  | `Eq -> Eq | `Neq -> Neq | `Geq -> Ge | `Gt -> Gt | `Leq -> Le | `Lt -> Lt

let warned = Hashtbl.create 8

let warn_once key fmt =
  if Hashtbl.mem warned key then Format.ifprintf Format.err_formatter fmt
  else begin
    Hashtbl.replace warned key ();
    Format.eprintf fmt
  end

let rec filter_of_f : OpamTypes.filter -> filter = function
  | FBool b -> FBool b
  | FString _ -> FBool true (* a defined string in boolean position *)
  | FIdent (_, v, _) -> FVar (OpamVariable.to_string v)
  | FOp (FIdent (_, x, _), r, FString s) ->
      FCmp (OpamVariable.to_string x, rel_of_relop r, s)
  | FOp (FString s, r, FIdent (_, x, _)) ->
      FCmp (OpamVariable.to_string x, complement (rel_of_relop r), s)
  | FOp (FIdent (_, x, _), r, FIdent (_, y, _)) ->
      FCmp (OpamVariable.to_string x, rel_of_relop r, OpamVariable.to_string y)
  | FOp (_, _, _) -> FBool true
  | FAnd (a, b) -> FAnd (filter_of_f a, filter_of_f b)
  | FOr (a, b) -> FOr (filter_of_f a, filter_of_f b)
  | FNot f -> FNot (filter_of_f f)
  | FDefined _ -> FBool true
  | FUndef _ -> FBool false

let rec cond_filter :
    OpamTypes.filter OpamTypes.filter_or_constraint OpamFormula.formula -> filter = function
  | Empty -> FBool true
  | Atom (Filter f) -> filter_of_f f
  | Atom (Constraint (r, FString s)) -> FVer (rel_of_relop r, s)
  | Atom (Constraint (_, f)) ->
      warn_once "constraint" "warning: unresolved version constraint against %s\n%!"
        (OpamFilter.to_string f);
      FBool true
  | Block f -> cond_filter f
  | And (a, b) -> FAnd (cond_filter a, cond_filter b)
  | Or (a, b) -> FOr (cond_filter a, cond_filter b)

let rec formula_to_dep : OpamTypes.filtered_formula -> dep_formula = function
  | Empty -> DAnd []
  | Atom (n, cond) -> DAtom (OpamPackage.Name.to_string n, [ cond_filter cond ])
  | Block f -> formula_to_dep f
  | And (a, b) -> DAnd [ formula_to_dep a; formula_to_dep b ]
  | Or (a, b) -> DOr [ formula_to_dep a; formula_to_dep b ]

let parse_opam_file path name version =
  let opam =
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string path))
  in
  let env fv =
    match OpamVariable.Full.scope fv with
    | OpamVariable.Full.Global | OpamVariable.Full.Self -> (
        match OpamVariable.to_string (OpamVariable.Full.variable fv) with
        | "version" -> Some (OpamTypes.S version)
        | "name" -> Some (OpamTypes.S name)
        | _ -> None)
    | OpamVariable.Full.Package _ -> None
  in
  let pf f = OpamFilter.partial_eval env f in
  let partial_formula ff =
    OpamFormula.map
      (fun (n, cond) ->
        OpamFormula.Atom
          ( n,
            OpamFormula.map
              (function
                | OpamTypes.Filter f -> OpamFormula.Atom (OpamTypes.Filter (pf f))
                | OpamTypes.Constraint (r, f) ->
                    OpamFormula.Atom (OpamTypes.Constraint (r, pf f)))
              cond ))
      ff
  in
  {
    name;
    version;
    depends = formula_to_dep (partial_formula (OpamFile.OPAM.depends opam));
    conflicts = formula_to_dep (partial_formula (OpamFile.OPAM.conflicts opam));
    conflict_classes =
      List.map OpamPackage.Name.to_string (OpamFile.OPAM.conflict_class opam);
    available = filter_of_f (pf (OpamFile.OPAM.available opam));
  }

type t = {
  by_name : (string, opkg list) Hashtbl.t; (* newest first *)
  by_id : (string * string, opkg) Hashtbl.t;
  conflicts_against : (string, opkg) Hashtbl.t; (* target name -> declarers *)
  class_members : (string, opkg) Hashtbl.t; (* conflict-class -> members *)
  var_values : (string, string) Hashtbl.t; (* variable -> values compared against *)
}

let rec filter_vars f k =
  match f with
  | FBool _ | FVer _ -> ()
  | FVar x -> k (x, "true")
  | FCmp (x, _, y) -> k (x, y)
  | FNot g -> filter_vars g k
  | FAnd (a, b) | FOr (a, b) ->
      filter_vars a k;
      filter_vars b k

let rec formula_vars d k =
  match d with
  | DAtom (_, fs) -> List.iter (fun f -> filter_vars f k) fs
  | DAnd l | DOr l -> List.iter (fun x -> formula_vars x k) l

let load dir =
  let pkgs_dir = Filename.concat dir "packages" in
  let t =
    {
      by_name = Hashtbl.create 8192;
      by_id = Hashtbl.create 16384;
      conflicts_against = Hashtbl.create 1024;
      class_members = Hashtbl.create 64;
      var_values = Hashtbl.create 256;
    }
  in
  let names = Sys.readdir pkgs_dir in
  Array.sort compare names;
  Array.iter
    (fun name ->
      let name_dir = Filename.concat pkgs_dir name in
      if Sys.is_directory name_dir then
        Array.iter
          (fun sub ->
            let opam_file = Filename.concat (Filename.concat name_dir sub) "opam" in
            let prefix = name ^ "." in
            if String.starts_with ~prefix sub && Sys.file_exists opam_file then
              let version =
                String.sub sub (String.length prefix) (String.length sub - String.length prefix)
              in
              try
                let p = parse_opam_file opam_file name version in
                Hashtbl.replace t.by_id (name, version) p;
                Hashtbl.replace t.by_name name
                  (p :: Option.value ~default:[] (Hashtbl.find_opt t.by_name name));
                let rec conflict_targets = function
                  | DAtom (n, _) -> Hashtbl.add t.conflicts_against n p
                  | DAnd l | DOr l -> List.iter conflict_targets l
                in
                conflict_targets p.conflicts;
                List.iter (fun c -> Hashtbl.add t.class_members c p) p.conflict_classes;
                let record (x, v) = Hashtbl.add t.var_values x v in
                formula_vars p.depends record;
                formula_vars p.conflicts record;
                filter_vars p.available record
              with _ -> Printf.eprintf "warning: failed to parse %s\n%!" opam_file)
          (Sys.readdir name_dir))
    names;
  Hashtbl.filter_map_inplace
    (fun _ ps ->
      Some
        (List.sort
           (fun a b ->
             Deb_version.compare (Deb_version.parse b.version) (Deb_version.parse a.version))
           ps))
    t.by_name;
  t

let versions t name = Option.value ~default:[] (Hashtbl.find_opt t.by_name name)

let domain t x =
  Hashtbl.find_all t.var_values x |> List.sort_uniq compare
