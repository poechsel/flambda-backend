(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Floating-point arithmetic.

    OCaml's floating-point numbers follow the
    IEEE 754 standard, using single precision (32 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0s /. 0.0s],
    [neg_infinity] for [-1.0s /. 0.0s], and [nan] ('not a number')
    for [0.0s /. 0.0s].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0s /. infinity] is [0.0s], basic arithmetic operations
    ([+.], [-.], [*.], [/.]) with [nan] as an argument return [nan], ...
*)

type t = float32
(** An alias for the type of 32-bit floating-point numbers. *)

val zero : float32
(** The floating point 0.s *)

val one : float32
(** The floating-point 1.s *)

val minus_one : float32
(** The floating-point -1.s *)

external neg : (float32[@local_opt]) -> (float32[@local_opt]) = "%negfloat32"
(** Unary negation. *)

external add :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%addfloat32"
(** Floating-point addition. *)

external sub :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%subfloat32"
(** Floating-point subtraction. *)

external mul :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%mulfloat32"
(** Floating-point multiplication. *)

external div :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%divfloat32"
(** Floating-point division. *)

external pow : float32 -> float32 -> float32
  = "caml_power_float32_bytecode" "powf"
  [@@unboxed] [@@noalloc]
(** Exponentiation. *)

(** Floating-point arithmetic operator overloads. *)
module Operators : sig
  external ( ~-. ) : (float32[@local_opt]) -> (float32[@local_opt])
    = "%negfloat32"
  (** Unary negation. *)

  external ( +. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%addfloat32"
  (** Floating-point addition. *)

  external ( -. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%subfloat32"
  (** Floating-point subtraction. *)

  external ( *. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%mulfloat32"
  (** Floating-point multiplication. *)

  external ( /. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%divfloat32"
  (** Floating-point division. *)

  external ( ** ) : float32 -> float32 -> float32
    = "caml_power_float32_bytecode" "powf"
    [@@unboxed] [@@noalloc]
  (** Exponentiation. *)
end

external fma : float32 -> float32 -> float32 -> float32
  = "caml_fma_float32_bytecode" "fmaf"
  [@@unboxed] [@@noalloc]
(** [fma x y z] returns [x * y + z], with a best effort for computing
   this expression with a single rounding, using either hardware
   instructions (providing full IEEE compliance) or a software
   emulation. *)

external rem : float32 -> float32 -> float32
  = "caml_fmod_float32_bytecode" "fmodf"
  [@@unboxed] [@@noalloc]
(** [rem a b] returns the remainder of [a] with respect to [b].  The returned
    value is [a -. n *. b], where [n] is the quotient [a /. b] rounded towards
    zero to an integer. *)

val succ : float32 -> float32
(** [succ x] returns the floating point number right after [x] i.e.,
   the smallest floating-point number greater than [x].  See also
   {!next_after}. *)

val pred : float32 -> float32
(** [pred x] returns the floating-point number right before [x] i.e.,
   the greatest floating-point number smaller than [x].  See also
   {!next_after}. *)

external abs : (float32[@local_opt]) -> (float32[@local_opt]) = "%absfloat32"
(** [abs f] returns the absolute value of [f]. *)

val infinity : float32
(** Positive infinity. *)

val neg_infinity : float32
(** Negative infinity. *)

val nan : float32
(** A special floating-point value denoting the result of an
    undefined operation such as [0.0s /. 0.0s].  Stands for
    'not a number'.  Any floating-point operation with [nan] as
    argument returns [nan] as result, unless otherwise specified in
    IEEE 754 standard.  As for floating-point comparisons,
    [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
    if one or both of their arguments is [nan]. *)

val pi : float32
(** The constant pi. *)

val max_float : float32
(** The largest positive finite value of type [float32]. *)

val min_float : float32
(** The smallest positive, non-zero, non-denormalized value of type [float32]. *)

val epsilon : float32
(** The difference between [1.0s] and the smallest exactly representable
    floating-point number greater than [1.0s]. *)

val is_finite : float32 -> bool
(** [is_finite x] is [true] if and only if [x] is finite i.e., not infinite and
   not {!nan}. *)

val is_infinite : float32 -> bool
(** [is_infinite x] is [true] if and only if [x] is {!infinity} or
    {!neg_infinity}. *)

val is_nan : float32 -> bool
(** [is_nan x] is [true] if and only if [x] is not a number (see {!nan}). *)

val is_integer : float32 -> bool
(** [is_integer x] is [true] if and only if [x] is an integer. *)

external of_float : (float[@local_opt]) -> float32 = "%float32offloat"
(** Convert a 64-bit float to the nearest representable 32-bit float. *)

external to_float : (float32[@local_opt]) -> float = "%floatoffloat32"
(** Convert a 32-bit float to a 64-bit float. *)

external of_int : int -> float32 = "%float32ofint"
(** Convert an integer to floating-point. *)

external to_int : (float32[@local_opt]) -> int = "%intoffloat32"
(** Truncate the given floating-point number to an integer.
    The result is unspecified if the argument is [nan] or falls outside the
    range of representable integers. *)

external of_bits : (int32[@local_opt]) -> float32
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Convert a 32-bit float to a 32-bit integer, preserving the value's
    bit pattern.
    The amd64 flambda-backend compiler translates this call to MOVD. *)

external to_bits : (float32[@local_opt]) -> int32
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Convert a 32-bit integer to a 32-bit float, preserving the value's
    bit pattern.
    The amd64 flambda-backend compiler translates this call to MOVD. *)

external of_string : string -> float32 = "caml_float32_of_string"
(** Convert the given string to a float.  The string is read in decimal
    (by default) or in hexadecimal (marked by [0x] or [0X]).
    The format of decimal floating-point numbers is
    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.
    The format of hexadecimal floating-point numbers is
    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an
    hexadecimal digit and [d] for a decimal digit.
    In both cases, at least one of the integer and fractional parts must be
    given; the exponent part is optional.
    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Depending on the execution platforms, other representations of
    floating-point numbers can be accepted, but should not be relied upon.
    @raise Failure if the given string is not a valid
    representation of a float32. *)

val of_string_opt : string -> float32 option
(** Same as [of_string], but returns [None] instead of raising. *)

val to_string : float32 -> string
(** Return a string representation of a floating-point number.

    This conversion does not involve a loss of precision. *)

(** The five classes of floating-point numbers, as determined by
    the {!classify_float} function. *)
type fpclass = Stdlib.fpclass =
  | FP_normal  (** Normal number, none of the below *)
  | FP_subnormal  (** Number very close to 0.0s, has reduced precision *)
  | FP_zero  (** Number is 0.0s or -0.0s *)
  | FP_infinite  (** Number is positive or negative infinity *)
  | FP_nan  (** Not a number: result of an undefined operation *)

external classify_float : (float32[@unboxed]) -> fpclass
  = "caml_classify_float32_bytecode" "caml_classify_float32"
  [@@noalloc]
(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)

external sqrt : float32 -> float32 = "caml_sqrt_float32_bytecode" "sqrtf"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Square root.
    The amd64 flambda-backend compiler translates this call to SQRTSS. *)

external cbrt : float32 -> float32 = "caml_cbrt_float32_bytecode" "cbrtf"
  [@@unboxed] [@@noalloc]
(** Cube root. *)

external exp : float32 -> float32 = "caml_exp_float32_bytecode" "expf"
  [@@unboxed] [@@noalloc]
(** Exponential. *)

external exp2 : float32 -> float32 = "caml_exp2_float32_bytecode" "exp2f"
  [@@unboxed] [@@noalloc]
(** Base 2 exponential function. *)

external log : float32 -> float32 = "caml_log_float32_bytecode" "logf"
  [@@unboxed] [@@noalloc]
(** Natural logarithm. *)

external log10 : float32 -> float32 = "caml_log10_float32_bytecode" "log10f"
  [@@unboxed] [@@noalloc]
(** Base 10 logarithm. *)

external log2 : float32 -> float32 = "caml_log2_float32_bytecode" "log2f"
  [@@unboxed] [@@noalloc]
(** Base 2 logarithm. *)

external expm1 : float32 -> float32 = "caml_expm1_float32_bytecode" "expm1f"
  [@@unboxed] [@@noalloc]
(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0]. *)

external log1p : float32 -> float32 = "caml_log1p_float32_bytecode" "log1pf"
  [@@unboxed] [@@noalloc]
(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0]. *)

external cos : float32 -> float32 = "caml_cos_float32_bytecode" "cosf"
  [@@unboxed] [@@noalloc]
(** Cosine.  Argument is in radians. *)

external sin : float32 -> float32 = "caml_sin_float32_bytecode" "sinf"
  [@@unboxed] [@@noalloc]
(** Sine.  Argument is in radians. *)

external tan : float32 -> float32 = "caml_tan_float32_bytecode" "tanf"
  [@@unboxed] [@@noalloc]
(** Tangent.  Argument is in radians. *)

external acos : float32 -> float32 = "caml_acos_float32_bytecode" "acosf"
  [@@unboxed] [@@noalloc]
(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [0.0] and [pi]. *)

external asin : float32 -> float32 = "caml_asin_float32_bytecode" "asinf"
  [@@unboxed] [@@noalloc]
(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan : float32 -> float32 = "caml_atan_float32_bytecode" "atanf"
  [@@unboxed] [@@noalloc]
(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan2 : float32 -> float32 -> float32
  = "caml_atan2_float32_bytecode" "atan2f"
  [@@unboxed] [@@noalloc]
(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)

external hypot : float32 -> float32 -> float32
  = "caml_hypot_float32_bytecode" "hypotf"
  [@@unboxed] [@@noalloc]
(** [hypot x y] returns [sqrt(x *. x +. y *. y)], that is, the length
    of the hypotenuse of a right-angled triangle with sides of length
    [x] and [y], or, equivalently, the distance of the point [(x,y)]
    to origin.  If one of [x] or [y] is infinite, returns [infinity]
    even if the other is [nan]. *)

external cosh : float32 -> float32 = "caml_cosh_float32_bytecode" "coshf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic cosine.  Argument is in radians. *)

external sinh : float32 -> float32 = "caml_sinh_float32_bytecode" "sinhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic sine.  Argument is in radians. *)

external tanh : float32 -> float32 = "caml_tanh_float32_bytecode" "tanhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic tangent.  Argument is in radians. *)

external acosh : float32 -> float32 = "caml_acosh_float32_bytecode" "acoshf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc cosine.  The argument must fall within the range
    [[1.0, inf]].
    Result is in radians and is between [0.0] and [inf]. *)

external asinh : float32 -> float32 = "caml_asinh_float32_bytecode" "asinhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc sine.  The argument and result range over the entire
    real line.
    Result is in radians. *)

external atanh : float32 -> float32 = "caml_atanh_float32_bytecode" "atanhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc tangent.  The argument must fall within the range
    [[-1.0, 1.0]].
    Result is in radians and ranges over the entire real line. *)

external erf : float32 -> float32 = "caml_erf_float32_bytecode" "erff"
  [@@unboxed] [@@noalloc]
(** Error function.  The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]]. *)

external erfc : float32 -> float32 = "caml_erfc_float32_bytecode" "erfcf"
  [@@unboxed] [@@noalloc]
(** Complementary error function ([erfc x = 1 - erf x]).
    The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]]. *)

external trunc : float32 -> float32 = "caml_trunc_float32_bytecode" "truncf"
  [@@unboxed] [@@noalloc]
(** [trunc x] rounds [x] to the nearest integer whose absolute value is
   less than or equal to [x]. *)

external round : float32 -> float32 = "caml_round_float32_bytecode" "roundf"
  [@@unboxed] [@@noalloc]
(** [round x] rounds [x] to the nearest integer with ties (fractional
   values of 0.5s) rounded away from zero, regardless of the current
   rounding direction.  If [x] is an integer, [+0.s], [-0.s], [nan], or
   infinite, [x] itself is returned.

   On 64-bit mingw-w64, this function may be emulated owing to a bug in the
   C runtime library (CRT) on this platform. *)

external ceil : float32 -> float32 = "caml_ceil_float32_bytecode" "ceilf"
  [@@unboxed] [@@noalloc]
(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float32. *)

external floor : float32 -> float32 = "caml_floor_float32_bytecode" "floorf"
  [@@unboxed] [@@noalloc]
(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float32. *)

external next_after : float32 -> float32 -> float32
  = "caml_nextafter_float32_bytecode" "nextafterf"
  [@@unboxed] [@@noalloc]
(** [next_after x y] returns the next representable floating-point
   value following [x] in the direction of [y].  More precisely, if
   [y] is greater (resp. less) than [x], it returns the smallest
   (resp. largest) representable number greater (resp. less) than [x].
   If [x] equals [y], the function returns [y].  If [x] or [y] is
   [nan], a [nan] is returned.
   Note that [next_after max_float infinity = infinity] and that
   [next_after 0.s infinity] is the smallest denormalized positive number.
   If [x] is the smallest denormalized positive number,
   [next_after x 0.s = 0.s] *)

external copy_sign : float32 -> float32 -> float32
  = "caml_copysign_float32_bytecode" "copysignf"
  [@@unboxed] [@@noalloc]
(** [copy_sign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which. *)

external sign_bit : (float32[@unboxed]) -> bool
  = "caml_signbit_float32_bytecode" "caml_signbit_float32"
  [@@noalloc]
(** [sign_bit x] is [true] if and only if the sign bit of [x] is set.
    For example [sign_bit 1.] and [signbit 0.] are [false] while
    [sign_bit (-1.)] and [sign_bit (-0.)] are [true]. *)

external frexp : float32 -> float32 * int = "caml_frexp_float32"
(** [frexp f] returns the pair of the significant
    and the exponent of [f].  When [f] is zero, the
    significant [x] and the exponent [n] of [f] are equal to
    zero.  When [f] is non-zero, they are defined by
    [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)

external ldexp : (float32[@unboxed]) -> (int[@untagged]) -> (float32[@unboxed])
  = "caml_ldexp_float32_bytecode" "caml_ldexp_float32"
  [@@noalloc]
(** [ldexp x n] returns [x *. 2 ** n]. *)

external modf : float32 -> float32 * float32 = "caml_modf_float32"
(** [modf f] returns the pair of the fractional and integral
    part of [f]. *)

val compare : float32 -> float32 -> int
(** [compare x y] returns [0] if [x] is equal to [y], a negative integer if [x]
    is less than [y], and a positive integer if [x] is greater than
    [y]. [compare] treats [nan] as equal to itself and less than any other float
    value.  This treatment of [nan] ensures that [compare] defines a total
    ordering relation.  *)

val equal : float32 -> float32 -> bool
(** The equal function for floating-point numbers, compared using {!compare}. *)

val min : float32 -> float32 -> float32
(** [min x y] returns the minimum of [x] and [y].  It returns [nan]
   when [x] or [y] is [nan].  Moreover [min (-0.s) (+0.s) = -0.s] *)

val max : float32 -> float32 -> float32
(** [max x y] returns the maximum of [x] and [y].  It returns [nan]
   when [x] or [y] is [nan].  Moreover [max (-0.s) (+0.s) = +0.s] *)

val min_max : float32 -> float32 -> float32 * float32
(** [min_max x y] is [(min x y, max x y)], just more efficient. *)

val min_num : float32 -> float32 -> float32
(** [min_num x y] returns the minimum of [x] and [y] treating [nan] as
   missing values.  If both [x] and [y] are [nan], [nan] is returned.
   Moreover [min_num (-0.s) (+0.s) = -0.s] *)

val max_num : float32 -> float32 -> float32
(** [max_num x y] returns the maximum of [x] and [y] treating [nan] as
   missing values.  If both [x] and [y] are [nan] [nan] is returned.
   Moreover [max_num (-0.s) (+0.s) = +0.s] *)

val min_max_num : float32 -> float32 -> float32 * float32
(** [min_max_num x y] is [(min_num x y, max_num x y)], just more
   efficient.  Note that in particular [min_max_num x nan = (x, x)]
   and [min_max_num nan y = (y, y)]. *)

val seeded_hash : int -> float32 -> int
(** A seeded hash function for floats, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)

val hash : float32 -> int
(** An unseeded hash function for floats, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
