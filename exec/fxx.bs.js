// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Lib = require("../util/lib.bs.js");
var Caml = require("rescript/lib/js/caml.js");
var Curry = require("rescript/lib/js/curry.js");
var Int64 = require("rescript/lib/js/int64.js");
var $$Buffer = require("rescript/lib/js/buffer.js");
var Printf = require("rescript/lib/js/printf.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");
var Caml_int64 = require("rescript/lib/js/caml_int64.js");
var Pervasives = require("rescript/lib/js/pervasives.js");
var Caml_format = require("rescript/lib/js/caml_format.js");
var Caml_string = require("rescript/lib/js/caml_string.js");

function Make(Rep) {
  if (Rep.mantissa > 52) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "fxx.ml",
            67,
            10
          ],
          Error: new Error()
        };
  }
  var pos_inf = Curry._1(Rep.bits_of_float, 1.0 / 0.0);
  var neg_inf = Curry._1(Rep.bits_of_float, -(1.0 / 0.0));
  var pos_nan = Rep.pos_nan;
  var bare_nan = Rep.bare_nan;
  var of_float = Rep.bits_of_float;
  var to_float = Rep.float_of_bits;
  var of_bits = function (x) {
    return x;
  };
  var to_bits = function (x) {
    return x;
  };
  var is_inf = function (x) {
    if (Caml_obj.caml_equal(x, pos_inf)) {
      return true;
    } else {
      return Caml_obj.caml_equal(x, neg_inf);
    }
  };
  var is_nan = function (x) {
    var xf = Curry._1(Rep.float_of_bits, x);
    return xf !== xf;
  };
  var determine_binary_nan = function (x, y) {
    var x$1 = is_nan(x) ? x : (
        is_nan(y) ? y : Rep.pos_nan
      );
    return Curry._2(Rep.logor, x$1, Rep.pos_nan);
  };
  var determine_unary_nan = function (x) {
    var x$1 = is_nan(x) ? x : Rep.pos_nan;
    return Curry._2(Rep.logor, x$1, Rep.pos_nan);
  };
  var binary = function (x, op, y) {
    var xf = Curry._1(to_float, x);
    var yf = Curry._1(to_float, y);
    var t = Curry._2(op, xf, yf);
    if (t === t) {
      return Curry._1(of_float, t);
    } else {
      return determine_binary_nan(x, y);
    }
  };
  var unary = function (op, x) {
    var t = Curry._1(op, Curry._1(to_float, x));
    if (t === t) {
      return Curry._1(of_float, t);
    } else {
      return determine_unary_nan(x);
    }
  };
  var zero = Curry._1(of_float, 0.0);
  var add = function (x, y) {
    return binary(x, (function (prim0, prim1) {
                  return prim0 + prim1;
                }), y);
  };
  var sub = function (x, y) {
    return binary(x, (function (prim0, prim1) {
                  return prim0 - prim1;
                }), y);
  };
  var mul = function (x, y) {
    return binary(x, (function (prim0, prim1) {
                  return prim0 * prim1;
                }), y);
  };
  var div = function (x, y) {
    return binary(x, (function (prim0, prim1) {
                  return prim0 / prim1;
                }), y);
  };
  var sqrt = function (x) {
    return unary((function (prim) {
                  return Math.sqrt(prim);
                }), x);
  };
  var ceil = function (x) {
    return unary((function (prim) {
                  return Math.ceil(prim);
                }), x);
  };
  var floor = function (x) {
    return unary((function (prim) {
                  return Math.floor(prim);
                }), x);
  };
  var trunc = function (x) {
    var xf = Curry._1(to_float, x);
    if (xf === 0.0) {
      return x;
    }
    var f = xf < 0.0 ? Math.ceil(xf) : Math.floor(xf);
    var result = Curry._1(of_float, f);
    if (is_nan(result)) {
      return determine_unary_nan(result);
    } else {
      return result;
    }
  };
  var nearest = function (x) {
    var xf = Curry._1(to_float, x);
    if (xf === 0.0) {
      return x;
    }
    var u = Math.ceil(xf);
    var d = Math.floor(xf);
    var um = Math.abs(xf - u);
    var dm = Math.abs(xf - d);
    var u_or_d = true;
    if (um >= dm) {
      var tmp = false;
      if (um === dm) {
        var h = u / 2;
        tmp = Math.floor(h) === h;
      }
      u_or_d = tmp;
    }
    var f = u_or_d ? u : d;
    var result = Curry._1(of_float, f);
    if (is_nan(result)) {
      return determine_unary_nan(result);
    } else {
      return result;
    }
  };
  var min = function (x, y) {
    var xf = Curry._1(to_float, x);
    var yf = Curry._1(to_float, y);
    if (xf === yf) {
      return Curry._2(Rep.logor, x, y);
    } else if (xf < yf) {
      return x;
    } else if (xf > yf) {
      return y;
    } else {
      return determine_binary_nan(x, y);
    }
  };
  var max = function (x, y) {
    var xf = Curry._1(to_float, x);
    var yf = Curry._1(to_float, y);
    if (xf === yf) {
      return Curry._2(Rep.logand, x, y);
    } else if (xf > yf) {
      return x;
    } else if (xf < yf) {
      return y;
    } else {
      return determine_binary_nan(x, y);
    }
  };
  var abs = function (x) {
    return Curry._2(Rep.logand, x, Rep.max_int);
  };
  var neg = function (x) {
    return Curry._2(Rep.logxor, x, Rep.min_int);
  };
  var copysign = function (x, y) {
    return Curry._2(Rep.logor, Curry._2(Rep.logand, x, Rep.max_int), Curry._2(Rep.logand, y, Rep.min_int));
  };
  var eq = function (x, y) {
    return Curry._1(to_float, x) === Curry._1(to_float, y);
  };
  var ne = function (x, y) {
    return Curry._1(to_float, x) !== Curry._1(to_float, y);
  };
  var lt = function (x, y) {
    return Curry._1(to_float, x) < Curry._1(to_float, y);
  };
  var gt = function (x, y) {
    return Curry._1(to_float, x) > Curry._1(to_float, y);
  };
  var le = function (x, y) {
    return Curry._1(to_float, x) <= Curry._1(to_float, y);
  };
  var ge = function (x, y) {
    return Curry._1(to_float, x) >= Curry._1(to_float, y);
  };
  var is_hex = function (c) {
    if (/* '0' */48 <= c && c <= /* '9' */57) {
      return true;
    } else if (/* 'A' */65 <= c) {
      return c <= /* 'F' */70;
    } else {
      return false;
    }
  };
  var at_end = function (hex, s, i) {
    if (i === s.length) {
      return true;
    } else {
      var c = Caml_string.get(s, i);
      return c === (
              hex ? /* 'P' */80 : /* 'E' */69
            );
    }
  };
  var skip_non_hex = function (s, _i) {
    while(true) {
      var i = _i;
      if (at_end(true, s, i) || is_hex(Caml_string.get(s, i))) {
        return i;
      }
      _i = i + 1 | 0;
      continue ;
    };
  };
  var skip_zeroes = function (s, _i) {
    while(true) {
      var i = _i;
      var i$p = skip_non_hex(s, i);
      if (at_end(true, s, i$p) || Caml_string.get(s, i$p) !== /* '0' */48) {
        return i$p;
      }
      _i = i$p + 1 | 0;
      continue ;
    };
  };
  var compare_mantissa_str = function (hex, s1, s2) {
    var s1$p = $$String.uppercase_ascii(s1);
    var s2$p = $$String.uppercase_ascii(s2);
    var _i1 = skip_zeroes(s1$p, 0);
    var _i2 = skip_zeroes(s2$p, 0);
    while(true) {
      var i2 = _i2;
      var i1 = _i1;
      var i1$p = skip_non_hex(s1$p, i1);
      var i2$p = skip_non_hex(s2$p, i2);
      var match = at_end(hex, s1$p, i1$p);
      var match$1 = at_end(hex, s2$p, i2$p);
      if (match) {
        if (match$1 || at_end(hex, s2$p, skip_zeroes(s2$p, i2$p))) {
          return 0;
        } else {
          return -1;
        }
      }
      if (match$1) {
        if (at_end(hex, s1$p, skip_zeroes(s1$p, i1$p))) {
          return 0;
        } else {
          return 1;
        }
      }
      var n = Caml.caml_int_compare(Caml_string.get(s1$p, i1$p), Caml_string.get(s2$p, i2$p));
      if (n !== 0) {
        return n;
      }
      _i2 = i2$p + 1 | 0;
      _i1 = i1$p + 1 | 0;
      continue ;
    };
  };
  var float_of_string_prevent_double_rounding = function (s) {
    var z = Caml_format.caml_float_of_string(s);
    if (Math.abs(z) === 1.0 / 0.0) {
      return z;
    }
    var bits = Caml_int64.bits_of_float(z);
    var lsb = Caml_int64.lsl_(Caml_int64.one, 52 - Rep.mantissa | 0);
    var tie = Caml_int64.asr_(lsb, 1);
    var mask = Int64.lognot(Caml_int64.lsl_(Caml_int64.neg_one, 52 - Rep.mantissa | 0));
    if (Caml.i64_neq(Caml_int64.and_(bits, mask), tie)) {
      return z;
    }
    var exp = Caml_int64.float_of_bits(Caml_int64.and_(bits, [
              -1048576,
              0
            ]));
    var eps = Caml_int64.float_of_bits(Caml_int64.or_(tie, Caml_int64.bits_of_float(exp))) - exp;
    var hex = $$String.contains(s, /* 'x' */120);
    var s$p;
    if (hex) {
      var m = Caml_int64.or_(Caml_int64.and_(bits, [
                1048575,
                4294967295
              ]), [
            1048576,
            0
          ]);
      var i = skip_zeroes($$String.uppercase_ascii(s), 0);
      if (i === s.length) {
        s$p = Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Float */8,
                    _0: /* Float_g */9,
                    _1: /* No_padding */0,
                    _2: /* Arg_precision */1,
                    _3: /* End_of_format */0
                  },
                  _1: "%.*g"
                }), s.length, z);
      } else {
        var match = Caml_string.get(s, i);
        var sh = match > 55 || match < 50 ? (
            match !== 49 ? 3 : 0
          ) : (
            match >= 52 ? 2 : 1
          );
        s$p = Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Int64 */7,
                    _0: /* Int_x */6,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: /* End_of_format */0
                  },
                  _1: "%Lx"
                }), Caml_int64.lsl_(m, sh));
      }
    } else {
      s$p = Curry._2(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* Float */8,
                  _0: /* Float_g */9,
                  _1: /* No_padding */0,
                  _2: /* Arg_precision */1,
                  _3: /* End_of_format */0
                },
                _1: "%.*g"
              }), s.length, z);
    }
    var match$1 = compare_mantissa_str(hex, s, s$p);
    switch (match$1) {
      case -1 :
          return z - eps;
      case 0 :
          return z;
      case 1 :
          return z + eps;
      default:
        return z;
    }
  };
  var of_signless_string = function (s) {
    if (s === "inf") {
      return pos_inf;
    }
    if (s === "nan") {
      return pos_nan;
    }
    if (s.length > 6 && $$String.sub(s, 0, 6) === "nan:0x") {
      var x = Curry._1(Rep.of_string, $$String.sub(s, 4, s.length - 4 | 0));
      if (Caml_obj.caml_equal(x, Rep.zero)) {
        throw {
              RE_EXN_ID: "Failure",
              _1: "nan payload must not be zero",
              Error: new Error()
            };
      }
      if (Caml_obj.caml_notequal(Curry._2(Rep.logand, x, bare_nan), Rep.zero)) {
        throw {
              RE_EXN_ID: "Failure",
              _1: "nan payload must not overlap with exponent bits",
              Error: new Error()
            };
      }
      if (Caml_obj.caml_lessthan(x, Rep.zero)) {
        throw {
              RE_EXN_ID: "Failure",
              _1: "nan payload must not overlap with sign bit",
              Error: new Error()
            };
      }
      return Curry._2(Rep.logor, x, bare_nan);
    }
    var s$p = $$String.concat("", $$String.split_on_char(/* '_' */95, s));
    var x$1 = Curry._1(of_float, float_of_string_prevent_double_rounding(s$p));
    if (is_inf(x$1)) {
      return Pervasives.failwith("of_string");
    } else {
      return x$1;
    }
  };
  var of_string = function (s) {
    if (s === "") {
      return Pervasives.failwith("of_string");
    }
    if (!(Caml_string.get(s, 0) === /* '+' */43 || Caml_string.get(s, 0) === /* '-' */45)) {
      return of_signless_string(s);
    }
    var x = of_signless_string($$String.sub(s, 1, s.length - 1 | 0));
    if (Caml_string.get(s, 0) === /* '+' */43) {
      return x;
    } else {
      return Curry._2(Rep.logxor, x, Rep.min_int);
    }
  };
  var is_digit = function (c) {
    if (/* '0' */48 <= c) {
      return c <= /* '9' */57;
    } else {
      return false;
    }
  };
  var is_hex_digit = function (c) {
    if (is_digit(c)) {
      return true;
    } else if (/* 'a' */97 <= c) {
      return c <= /* 'f' */102;
    } else {
      return false;
    }
  };
  var add_digits = function (buf, s, _i, j, _k, n) {
    while(true) {
      var k = _k;
      var i = _i;
      if (i >= j) {
        return ;
      }
      if (k === 0) {
        $$Buffer.add_char(buf, /* '_' */95);
      }
      $$Buffer.add_char(buf, Caml_string.get(s, i));
      _k = Caml_int32.mod_((k + n | 0) - 1 | 0, n);
      _i = i + 1 | 0;
      continue ;
    };
  };
  var group_digits = function (is_digit, n, s) {
    var isnt_digit = function (c) {
      return !Curry._1(is_digit, c);
    };
    var len = s.length;
    var x = Lib.$$Option.get(Lib.$$String.find_from_opt((function (param) {
                return /* 'x' */120 === param;
              }), s, 0), 0);
    var mant = Lib.$$Option.get(Lib.$$String.find_from_opt(is_digit, s, x), len);
    var point = Lib.$$Option.get(Lib.$$String.find_from_opt(isnt_digit, s, mant), len);
    var frac = Lib.$$Option.get(Lib.$$String.find_from_opt(is_digit, s, point), len);
    var exp = Lib.$$Option.get(Lib.$$String.find_from_opt(isnt_digit, s, frac), len);
    var buf = $$Buffer.create(Caml_int32.div(Math.imul(len, n + 1 | 0), n));
    $$Buffer.add_substring(buf, s, 0, mant);
    add_digits(buf, s, mant, point, Caml_int32.mod_(point - mant | 0, n) + n | 0, n);
    $$Buffer.add_substring(buf, s, point, frac - point | 0);
    add_digits(buf, s, frac, exp, n, n);
    $$Buffer.add_substring(buf, s, exp, len - exp | 0);
    return $$Buffer.contents(buf);
  };
  var to_string$p = function (convert, is_digit, n, x) {
    var tmp;
    if (is_nan(x)) {
      var payload = Curry._2(Rep.logand, Curry._2(Rep.logand, x, Rep.max_int), Curry._1(Rep.lognot, bare_nan));
      tmp = "nan:0x" + group_digits(is_hex_digit, 4, Curry._1(Rep.to_hex_string, payload));
    } else {
      var s = Curry._1(convert, Curry._1(to_float, Curry._2(Rep.logand, x, Rep.max_int)));
      tmp = group_digits(is_digit, n, Caml_string.get(s, s.length - 1 | 0) === /* '.' */46 ? s + "0" : s);
    }
    return (
            Caml_obj.caml_lessthan(x, Rep.zero) ? "-" : ""
          ) + tmp;
  };
  var partial_arg = Printf.sprintf(/* Format */{
        _0: {
          TAG: /* Float */8,
          _0: /* Float_g */9,
          _1: /* No_padding */0,
          _2: /* Lit_precision */{
            _0: 17
          },
          _3: /* End_of_format */0
        },
        _1: "%.17g"
      });
  var to_string = function (param) {
    return to_string$p(partial_arg, is_digit, 3, param);
  };
  var to_hex_string = function (x) {
    if (is_inf(x)) {
      return Curry._1(to_string, x);
    } else {
      return to_string$p(Printf.sprintf(/* Format */{
                      _0: {
                        TAG: /* Float */8,
                        _0: /* Float_h */16,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: /* End_of_format */0
                      },
                      _1: "%h"
                    }), is_hex_digit, 4, x);
    }
  };
  return {
          pos_nan: pos_nan,
          neg_nan: Rep.neg_nan,
          is_inf: is_inf,
          is_nan: is_nan,
          of_float: of_float,
          to_float: to_float,
          of_string: of_string,
          to_string: to_string,
          to_hex_string: to_hex_string,
          of_bits: of_bits,
          to_bits: to_bits,
          add: add,
          sub: sub,
          mul: mul,
          div: div,
          sqrt: sqrt,
          min: min,
          max: max,
          ceil: ceil,
          floor: floor,
          trunc: trunc,
          nearest: nearest,
          abs: abs,
          neg: neg,
          copysign: copysign,
          eq: eq,
          ne: ne,
          lt: lt,
          le: le,
          gt: gt,
          ge: ge,
          zero: zero
        };
}

exports.Make = Make;
/* No side effect */
