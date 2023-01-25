external collect_gc_timings : unit -> unit = "caml_timing_collect_gc" [@@noalloc]
external gc_minor_ns : unit -> float = "caml_timing_gc_time_spend_minor"
external gc_major_ns : unit -> float = "caml_timing_gc_time_spend_major"

let start_collection () =
  collect_gc_timings ()

let print ppf =
  let precision = 3 in
  Format.fprintf ppf "minor_ns %0.*f\n" precision (gc_minor_ns () *. 1e-9);
  Format.fprintf ppf "major_ns %0.*f\n" precision (gc_major_ns () *. 1e-9)