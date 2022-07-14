// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var I32 = require("../exec/i32.bs.js");
var Lib = require("../util/lib.bs.js");
var List = require("rescript/lib/js/list.js");
var Curry = require("rescript/lib/js/curry.js");
var $$String = require("rescript/lib/js/string.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");

function num_size(param) {
  if (param !== 1 && param < 3) {
    return 4;
  } else {
    return 8;
  }
}

function vec_size(param) {
  return 16;
}

function packed_size(param) {
  switch (param) {
    case /* Pack8 */0 :
        return 1;
    case /* Pack16 */1 :
        return 2;
    case /* Pack32 */2 :
        return 4;
    case /* Pack64 */3 :
        return 8;
    
  }
}

function packed_shape_size(param) {
  return 8;
}

function is_num_type(param) {
  switch (param.TAG | 0) {
    case /* NumType */0 :
        return true;
    case /* VecType */1 :
    case /* RefType */2 :
        return false;
    
  }
}

function is_vec_type(param) {
  switch (param.TAG | 0) {
    case /* VecType */1 :
        return true;
    case /* NumType */0 :
    case /* RefType */2 :
        return false;
    
  }
}

function is_ref_type(param) {
  switch (param.TAG | 0) {
    case /* NumType */0 :
    case /* VecType */1 :
        return false;
    case /* RefType */2 :
        return true;
    
  }
}

function funcs(param) {
  return Lib.List.map_filter((function (t) {
                if (t.TAG === /* ExternFuncType */0) {
                  return t._0;
                }
                
              }), param);
}

function tables(param) {
  return Lib.List.map_filter((function (t) {
                if (t.TAG === /* ExternTableType */1) {
                  return t._0;
                }
                
              }), param);
}

function memories(param) {
  return Lib.List.map_filter((function (t) {
                if (t.TAG === /* ExternMemoryType */2) {
                  return t._0;
                }
                
              }), param);
}

function globals(param) {
  return Lib.List.map_filter((function (t) {
                if (t.TAG === /* ExternGlobalType */3) {
                  return t._0;
                }
                
              }), param);
}

function match_limits(lim1, lim2) {
  if (!Curry._2(I32.ge_u, lim1.min, lim2.min)) {
    return false;
  }
  var match = lim1.max;
  var match$1 = lim2.max;
  if (match$1 !== undefined) {
    if (match !== undefined) {
      return Curry._2(I32.le_u, match, match$1);
    } else {
      return false;
    }
  } else {
    return true;
  }
}

var match_func_type = Caml_obj.caml_equal;

function match_table_type(param, param$1) {
  if (param._1 === param$1._1) {
    return match_limits(param._0, param$1._0);
  } else {
    return false;
  }
}

function match_memory_type(lim1, lim2) {
  return match_limits(lim1._0, lim2._0);
}

var match_global_type = Caml_obj.caml_equal;

function match_extern_type(et1, et2) {
  switch (et1.TAG | 0) {
    case /* ExternFuncType */0 :
        if (et2.TAG === /* ExternFuncType */0) {
          return Caml_obj.caml_equal(et1._0, et2._0);
        } else {
          return false;
        }
    case /* ExternTableType */1 :
        if (et2.TAG === /* ExternTableType */1) {
          return match_table_type(et1._0, et2._0);
        } else {
          return false;
        }
    case /* ExternMemoryType */2 :
        if (et2.TAG === /* ExternMemoryType */2) {
          return match_memory_type(et1._0, et2._0);
        } else {
          return false;
        }
    case /* ExternGlobalType */3 :
        if (et2.TAG === /* ExternGlobalType */3) {
          return Caml_obj.caml_equal(et1._0, et2._0);
        } else {
          return false;
        }
    
  }
}

function string_of_num_type(param) {
  switch (param) {
    case /* I32Type */0 :
        return "i32";
    case /* I64Type */1 :
        return "i64";
    case /* F32Type */2 :
        return "f32";
    case /* F64Type */3 :
        return "f64";
    
  }
}

function string_of_vec_type(param) {
  return "v128";
}

function string_of_ref_type(param) {
  if (param) {
    return "externref";
  } else {
    return "funcref";
  }
}

function string_of_refed_type(param) {
  if (param) {
    return "extern";
  } else {
    return "func";
  }
}

function string_of_value_type(t) {
  switch (t.TAG | 0) {
    case /* NumType */0 :
        return string_of_num_type(t._0);
    case /* VecType */1 :
        return "v128";
    case /* RefType */2 :
        if (t._0) {
          return "externref";
        } else {
          return "funcref";
        }
    
  }
}

function string_of_value_types(ts) {
  if (ts && !ts.tl) {
    return string_of_value_type(ts.hd);
  }
  return "[" + ($$String.concat(" ", List.map(string_of_value_type, ts)) + "]");
}

function string_of_limits(param) {
  var max = param.max;
  return Curry._1(I32.to_string_u, param.min) + (
          max !== undefined ? " " + Curry._1(I32.to_string_u, max) : ""
        );
}

function string_of_memory_type(lim) {
  return string_of_limits(lim._0);
}

function string_of_table_type(param) {
  return string_of_limits(param._0) + (" " + (
            param._1 ? "externref" : "funcref"
          ));
}

function string_of_global_type(param) {
  var t = param._0;
  if (param._1) {
    return "(mut " + (string_of_value_type(t) + ")");
  } else {
    return string_of_value_type(t);
  }
}

function string_of_result_type(ts) {
  return "[" + ($$String.concat(" ", List.map(string_of_value_type, ts)) + "]");
}

function string_of_func_type(param) {
  return string_of_result_type(param._0) + (" -> " + string_of_result_type(param._1));
}

function string_of_extern_type(ft) {
  switch (ft.TAG | 0) {
    case /* ExternFuncType */0 :
        return "func " + string_of_func_type(ft._0);
    case /* ExternTableType */1 :
        return "table " + string_of_table_type(ft._0);
    case /* ExternMemoryType */2 :
        return "memory " + string_of_limits(ft._0._0);
    case /* ExternGlobalType */3 :
        return "global " + string_of_global_type(ft._0);
    
  }
}

exports.num_size = num_size;
exports.vec_size = vec_size;
exports.packed_size = packed_size;
exports.packed_shape_size = packed_shape_size;
exports.is_num_type = is_num_type;
exports.is_vec_type = is_vec_type;
exports.is_ref_type = is_ref_type;
exports.funcs = funcs;
exports.tables = tables;
exports.memories = memories;
exports.globals = globals;
exports.match_limits = match_limits;
exports.match_func_type = match_func_type;
exports.match_table_type = match_table_type;
exports.match_memory_type = match_memory_type;
exports.match_global_type = match_global_type;
exports.match_extern_type = match_extern_type;
exports.string_of_num_type = string_of_num_type;
exports.string_of_vec_type = string_of_vec_type;
exports.string_of_ref_type = string_of_ref_type;
exports.string_of_refed_type = string_of_refed_type;
exports.string_of_value_type = string_of_value_type;
exports.string_of_value_types = string_of_value_types;
exports.string_of_limits = string_of_limits;
exports.string_of_memory_type = string_of_memory_type;
exports.string_of_table_type = string_of_table_type;
exports.string_of_global_type = string_of_global_type;
exports.string_of_result_type = string_of_result_type;
exports.string_of_func_type = string_of_func_type;
exports.string_of_extern_type = string_of_extern_type;
/* I32 Not a pure module */
