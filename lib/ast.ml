type name = string
type version = string
type package = name * version
type dependency = package * (name * version list) list
type instance = dependency list
type query = (name * version list) list
type packages = package list

let pp_package fmt (n, v) = Format.fprintf fmt "%s %s" n v

let pp_target fmt (name, vs) =
  Format.fprintf fmt "%s (%a)" name
    Format.(pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ' ') pp_print_string)
    vs

let pp_dependency fmt (p, rs) =
  Format.fprintf fmt "%a%a;" pp_package p
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_char fmt ',')
        (fun fmt -> Format.fprintf fmt " -> %a" pp_target))
    rs

let pp fmt = Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_dependency fmt
