// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var F32 = require("./f32.bs.js");
var F64 = require("./f64.bs.js");
var Caml = require("rescript/lib/js/caml.js");
var Curry = require("rescript/lib/js/curry.js");
var Int64 = require("rescript/lib/js/int64.js");
var Caml_int64 = require("rescript/lib/js/caml_int64.js");
var I64_convert = require("./i64_convert.bs.js");

function promote_f32(x) {
  var xf = Curry._1(F32.to_float, x);
  if (xf === xf) {
    return Curry._1(F64.of_float, xf);
  }
  var nan32bits = I64_convert.extend_i32_u(Curry._1(F32.to_bits, x));
  var sign_field = Caml_int64.lsl_(Caml_int64.lsr_(nan32bits, 31), 63);
  var significand_field = Caml_int64.lsr_(Caml_int64.lsl_(nan32bits, 41), 12);
  var fields = Caml_int64.or_(sign_field, significand_field);
  return Curry._1(F64.of_bits, Caml_int64.or_([
                  2146959360,
                  0
                ], fields));
}

function convert_i32_s(x) {
  return Curry._1(F64.of_float, x);
}

function convert_i32_u(x) {
  return Curry._1(F64.of_float, Caml_int64.to_float(Caml_int64.and_(Caml_int64.of_int32(x), [
                      0,
                      4294967295
                    ])));
}

function convert_i64_s(x) {
  return Curry._1(F64.of_float, Caml_int64.to_float(x));
}

function convert_i64_u(x) {
  return Curry._1(F64.of_float, Caml.i64_ge(x, Int64.zero) ? Caml_int64.to_float(x) : Caml_int64.to_float(Caml_int64.or_(Caml_int64.lsr_(x, 1), Caml_int64.and_(x, Caml_int64.one))) * 2.0);
}

var reinterpret_i64 = F64.of_bits;

exports.promote_f32 = promote_f32;
exports.convert_i32_s = convert_i32_s;
exports.convert_i32_u = convert_i32_u;
exports.convert_i64_s = convert_i64_s;
exports.convert_i64_u = convert_i64_u;
exports.reinterpret_i64 = reinterpret_i64;
/* F32 Not a pure module */
