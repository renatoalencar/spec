// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Ast = require("../syntax/ast.bs.js");
var $$Map = require("rescript/lib/js/map.js");
var List = require("rescript/lib/js/list.js");
var Curry = require("rescript/lib/js/curry.js");
var $$Error = require("../util/error.bs.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

var Unknown = $$Error.Make({});

var compare = Caml_obj.caml_compare;

var Registry = $$Map.Make({
      compare: compare
    });

var registry = {
  contents: Registry.empty
};

function register(name, lookup) {
  registry.contents = Curry._3(Registry.add, name, lookup, registry.contents);
  
}

function link(m) {
  return List.map((function (param) {
                var match = param.it;
                var item_name = match.item_name;
                var module_name = match.module_name;
                var t = Ast.import_type(m, param);
                try {
                  return Curry._4(Registry.find, module_name, registry.contents, item_name, t);
                }
                catch (raw_exn){
                  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                  if (exn.RE_EXN_ID === "Not_found") {
                    return Curry._2(Unknown.error, param.at, "unknown import \"" + (Ast.string_of_name(module_name) + ("\".\"" + (Ast.string_of_name(item_name) + "\""))));
                  }
                  throw exn;
                }
              }), m.it.imports);
}

var Unknown$1 = Unknown.$$Error;

exports.Unknown = Unknown$1;
exports.link = link;
exports.register = register;
/* Unknown Not a pure module */
