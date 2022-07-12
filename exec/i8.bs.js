// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Ixx from "./ixx.bs.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Int32 from "rescript/lib/es6/int32.js";
import * as Printf from "rescript/lib/es6/printf.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";

function to_hex_string(i) {
  return Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Int32 */5,
                    _0: /* Int_x */6,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: /* End_of_format */0
                  },
                  _1: "%lx"
                }), i & 255);
}

var of_int64 = Caml_int64.to_int32;

var to_int64 = Caml_int64.of_int32;

var include = Ixx.Make({
      zero: Int32.zero,
      one: Int32.one,
      minus_one: Int32.minus_one,
      max_int: Int32.max_int,
      min_int: Int32.min_int,
      abs: Int32.abs,
      neg: (function (prim) {
          return -prim | 0;
        }),
      add: (function (prim0, prim1) {
          return prim0 + prim1 | 0;
        }),
      sub: (function (prim0, prim1) {
          return prim0 - prim1 | 0;
        }),
      mul: (function (prim0, prim1) {
          return Math.imul(prim0, prim1);
        }),
      div: Caml_int32.div,
      rem: Caml_int32.mod_,
      logand: (function (prim0, prim1) {
          return prim0 & prim1;
        }),
      lognot: Int32.lognot,
      logor: (function (prim0, prim1) {
          return prim0 | prim1;
        }),
      logxor: (function (prim0, prim1) {
          return prim0 ^ prim1;
        }),
      shift_left: (function (prim0, prim1) {
          return (prim0 << prim1);
        }),
      shift_right: (function (prim0, prim1) {
          return (prim0 >> prim1);
        }),
      shift_right_logical: (function (prim0, prim1) {
          return (prim0 >>> prim1) | 0;
        }),
      of_int: (function (prim) {
          return prim;
        }),
      to_int: (function (prim) {
          return prim;
        }),
      of_int64: of_int64,
      to_int64: to_int64,
      to_string: Int32.to_string,
      to_hex_string: to_hex_string,
      bitwidth: 8
    });

var of_bits = include.of_bits;

var to_bits = include.to_bits;

var zero = include.zero;

var lognot = include.lognot;

var abs = include.abs;

var neg = include.neg;

var add = include.add;

var sub = include.sub;

var mul = include.mul;

var div_s = include.div_s;

var div_u = include.div_u;

var rem_s = include.rem_s;

var rem_u = include.rem_u;

var avgr_u = include.avgr_u;

var and_ = include.and_;

var or_ = include.or_;

var xor = include.xor;

var shl = include.shl;

var shr_s = include.shr_s;

var shr_u = include.shr_u;

var rotl = include.rotl;

var rotr = include.rotr;

var clz = include.clz;

var ctz = include.ctz;

var popcnt = include.popcnt;

var extend_s = include.extend_s;

var eqz = include.eqz;

var eq = include.eq;

var ne = include.ne;

var lt_s = include.lt_s;

var lt_u = include.lt_u;

var le_s = include.le_s;

var le_u = include.le_u;

var gt_s = include.gt_s;

var gt_u = include.gt_u;

var ge_s = include.ge_s;

var ge_u = include.ge_u;

var as_unsigned = include.as_unsigned;

var saturate_s = include.saturate_s;

var saturate_u = include.saturate_u;

var add_sat_s = include.add_sat_s;

var add_sat_u = include.add_sat_u;

var sub_sat_s = include.sub_sat_s;

var sub_sat_u = include.sub_sat_u;

var q15mulr_sat_s = include.q15mulr_sat_s;

var of_int_s = include.of_int_s;

var of_int_u = include.of_int_u;

var of_string_s = include.of_string_s;

var of_string_u = include.of_string_u;

var of_string = include.of_string;

var to_int_s = include.to_int_s;

var to_int_u = include.to_int_u;

var to_string_s = include.to_string_s;

var to_string_u = include.to_string_u;

var to_hex_string$1 = include.to_hex_string;

export {
  of_bits ,
  to_bits ,
  zero ,
  lognot ,
  abs ,
  neg ,
  add ,
  sub ,
  mul ,
  div_s ,
  div_u ,
  rem_s ,
  rem_u ,
  avgr_u ,
  and_ ,
  or_ ,
  xor ,
  shl ,
  shr_s ,
  shr_u ,
  rotl ,
  rotr ,
  clz ,
  ctz ,
  popcnt ,
  extend_s ,
  eqz ,
  eq ,
  ne ,
  lt_s ,
  lt_u ,
  le_s ,
  le_u ,
  gt_s ,
  gt_u ,
  ge_s ,
  ge_u ,
  as_unsigned ,
  saturate_s ,
  saturate_u ,
  add_sat_s ,
  add_sat_u ,
  sub_sat_s ,
  sub_sat_u ,
  q15mulr_sat_s ,
  of_int_s ,
  of_int_u ,
  of_string_s ,
  of_string_u ,
  of_string ,
  to_int_s ,
  to_int_u ,
  to_string_s ,
  to_string_u ,
  to_hex_string$1 as to_hex_string,
  
}
/* include Not a pure module */
