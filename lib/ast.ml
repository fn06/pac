type yes = Yes
and no = N

type ('k, 'm) expr = ('k, 'k, 'm) expression

and ('deep, 'surface, 'm) expression =
  | [] : ('deep, < .. >, 'm) expression
  | ( :: ) :
      ('deep, 'm) expr * ('deep, 'm) expr
      -> ('deep, < .. >, 'm) expression
  | Dependency : dependency -> ('deep, < core : yes ; .. >, 'm) expression
  | Conflict : dependency -> ('deep, < conflicts : yes ; .. >, 'm) expression

and dependency = package * (name * version list) list
and name = string
and version = string
and package = name * version

let pp_package ppf (n, v) = Format.fprintf ppf "%s %s" n v

let pp_t ppf (name, vs) =
  Format.fprintf ppf "%s (%a)" name
    Format.(
      pp_print_list ~pp_sep:(fun f () -> pp_print_char f ' ') pp_print_string)
    vs

let pp_dep ppf (p, ts) =
  Format.fprintf ppf "%a (%a)" pp_package p
    Format.(pp_print_list ~pp_sep:(fun f () -> pp_print_char f ' ') pp_t)
    ts

let rec pp_expression ppf = function
  | [] -> ()
  | x :: xs ->
      pp_expression ppf x;
      Format.pp_print_newline ppf ();
      pp_expression ppf xs
  | Dependency d -> pp_dep ppf d
  | Conflict d -> pp_dep ppf d

let dependency d = Dependency d
let conflict d = Conflict d

let map (type a b) :
    ((a, 'm1) expr -> (b, 'm2) expr) -> (a, b, 'm1) expression -> (b, 'm2) expr
    =
 fun f -> function
  | [] -> []
  | x :: xs -> f x :: f xs
  | Dependency d -> Dependency d
  | Conflict c -> Conflict c

let fold (type a b) :
    f:((a, 'm1) expr -> 'acc -> 'acc) -> 'acc -> (a, 'm1) expr -> 'acc =
 fun ~f init -> function
  | [] -> init
  | x :: xs -> f xs (f x init)
  | Dependency _ -> init
  | Conflict _ -> init

type core = < conflicts : no ; core : yes >
type higher = < conflicts : yes ; core : yes >
type typed = Typed
type cexpr = (core, typed) expr
type hexpr = (higher, typed) expr

let all : hexpr =
  [
    dependency (("A", "1"), [ ("B", []) ]); conflict (("A", "1"), [ ("B", []) ]);
  ]

let rec pp_cexpr ppf (e : cexpr) =
  match e with
  | [] -> ()
  | x :: xs ->
      pp_cexpr ppf x;
      Format.pp_print_newline ppf ();
      pp_cexpr ppf xs
  | Dependency d -> pp_dep ppf d
