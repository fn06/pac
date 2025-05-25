type expression = dependency list
and dependency = package * (name * version list) list
and name = string
and version = string
and package = name * version

let pp_package ppf (n, v) = Format.fprintf ppf "%s %s" n v

let pp_t ppf (name, vs) =
  Format.fprintf ppf "%s (%a)" name Format.(pp_print_list pp_print_string) vs

let pp_dep ppf (p, ts) =
  Format.fprintf ppf "%a (%a)" pp_package p Format.(pp_print_list pp_t) ts

let pp ppf = Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_dep ppf

type yes = Yes and no = No

type 'k expr = ('k, 'k) expression

and ('deep, 'surface) expression =
  | [] : ('deep, 'surface) expression
  | (::) : ('deep, 'surface) expression * ('deep, 'surface) expression -> ('deep, 'surface) expression
  | Dependency : dep -> ('deep, < core: yes; ..>) expression
  | Conflict : dep -> ('deep, < conflicts: yes; ..>) expression
  | Concurrent : dep -> ('deep, < concurrent: yes; ..>) expression

and dep = package * (name * version list) list 

let dep : dep = (("A", "1"), [])

let core : < core : yes > expr = Dependency dep :: [ Dependency dep ]
let conflicts = Dependency dep :: [ Conflict dep ]
let concurrent = Dependency dep :: [ Concurrent dep ]
let all = Dependency dep :: [ Concurrent dep; Conflict dep ]

let rec shallow_map (type deep k1 k2) : ((deep, k1) expression -> (deep, k2) expression) -> (k1, k2) expression -> k2 expr = fun f -> function
  | [] -> []
  | x :: xs -> f x :: f xs
  | v -> f v

let rec encode_conflicts = function
  | Conflict d -> Dependency d
  | v -> shallow_map encode_conflicts v


let v = encode_conflicts core 
let v = encode_conflicts conflicts 
let v = encode_conflicts concurrent
let v = encode_conflicts all 


