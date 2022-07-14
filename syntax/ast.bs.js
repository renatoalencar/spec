// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Lib = require("../util/lib.bs.js");
var Char = require("rescript/lib/js/char.js");
var List = require("rescript/lib/js/list.js");
var Curry = require("rescript/lib/js/curry.js");
var Types = require("./types.bs.js");
var $$Buffer = require("rescript/lib/js/buffer.js");
var Printf = require("rescript/lib/js/printf.js");
var Pervasives = require("rescript/lib/js/pervasives.js");

var IntOp = {};

var FloatOp = {};

var V128Op = {};

function func_type_for(m, x) {
  return Lib.List32.nth(m.it.types, x.it).it;
}

function import_type(m, im) {
  var match = im.it;
  var x = match.idesc.it;
  switch (x.TAG | 0) {
    case /* FuncImport */0 :
        return {
                TAG: /* ExternFuncType */0,
                _0: func_type_for(m, x._0)
              };
    case /* TableImport */1 :
        return {
                TAG: /* ExternTableType */1,
                _0: x._0
              };
    case /* MemoryImport */2 :
        return {
                TAG: /* ExternMemoryType */2,
                _0: x._0
              };
    case /* GlobalImport */3 :
        return {
                TAG: /* ExternGlobalType */3,
                _0: x._0
              };
    
  }
}

function export_type(m, ex) {
  var match = ex.it;
  var its = List.map((function (param) {
          return import_type(m, param);
        }), m.it.imports);
  var x = match.edesc.it;
  switch (x.TAG | 0) {
    case /* FuncExport */0 :
        var fts = Pervasives.$at(Types.funcs(its), List.map((function (f) {
                    return func_type_for(m, f.it.ftype);
                  }), m.it.funcs));
        return {
                TAG: /* ExternFuncType */0,
                _0: Lib.List32.nth(fts, x._0.it)
              };
    case /* TableExport */1 :
        var tts = Pervasives.$at(Types.tables(its), List.map((function (t) {
                    return t.it.ttype;
                  }), m.it.tables));
        return {
                TAG: /* ExternTableType */1,
                _0: Lib.List32.nth(tts, x._0.it)
              };
    case /* MemoryExport */2 :
        var mts = Pervasives.$at(Types.memories(its), List.map((function (m) {
                    return m.it.mtype;
                  }), m.it.memories));
        return {
                TAG: /* ExternMemoryType */2,
                _0: Lib.List32.nth(mts, x._0.it)
              };
    case /* GlobalExport */3 :
        var gts = Pervasives.$at(Types.globals(its), List.map((function (g) {
                    return g.it.gtype;
                  }), m.it.globals));
        return {
                TAG: /* ExternGlobalType */3,
                _0: Lib.List32.nth(gts, x._0.it)
              };
    
  }
}

function string_of_name(n) {
  var b = $$Buffer.create(16);
  var $$escape = function (uc) {
    if (uc < 32 || uc >= 127) {
      return $$Buffer.add_string(b, Curry._1(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "\\u{",
                            _1: {
                              TAG: /* Int */4,
                              _0: /* Int_x */6,
                              _1: {
                                TAG: /* Lit_padding */0,
                                _0: /* Zeros */2,
                                _1: 2
                              },
                              _2: /* No_precision */0,
                              _3: {
                                TAG: /* Char_literal */12,
                                _0: /* '}' */125,
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "\\u{%02x}"
                        }), uc));
    }
    var c = Char.chr(uc);
    if (c === /* '"' */34 || c === /* '\\' */92) {
      $$Buffer.add_char(b, /* '\\' */92);
    }
    return $$Buffer.add_char(b, c);
  };
  List.iter($$escape, n);
  return $$Buffer.contents(b);
}

var I32Op;

var I64Op;

var F32Op;

var F64Op;

var empty_module = {
  types: /* [] */0,
  globals: /* [] */0,
  tables: /* [] */0,
  memories: /* [] */0,
  funcs: /* [] */0,
  start: undefined,
  elems: /* [] */0,
  datas: /* [] */0,
  imports: /* [] */0,
  exports: /* [] */0
};

exports.IntOp = IntOp;
exports.FloatOp = FloatOp;
exports.I32Op = I32Op;
exports.I64Op = I64Op;
exports.F32Op = F32Op;
exports.F64Op = F64Op;
exports.V128Op = V128Op;
exports.empty_module = empty_module;
exports.func_type_for = func_type_for;
exports.import_type = import_type;
exports.export_type = export_type;
exports.string_of_name = string_of_name;
/* Types Not a pure module */
