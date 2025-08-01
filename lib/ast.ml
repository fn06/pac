type name = string
type version = string
type package = name * version
type dependency = package * (name * version list) list
type expression = dependency list
type target = name * version list

let pp_package ppf (n, v) = Format.fprintf ppf "%s %s" n v

let pp_t ppf (name, vs) =
  Format.fprintf ppf "%s (%a)" name
    Format.(pp_print_list ~pp_sep:(fun f () -> pp_print_char f ' ') pp_print_string)
    vs

let pp_dep ppf (p, ts) =
  Format.fprintf ppf "%a (%a)" pp_package p
    Format.(pp_print_list ~pp_sep:(fun f () -> pp_print_char f ' ') pp_t)
    ts

let pp ppf = Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_dep ppf
