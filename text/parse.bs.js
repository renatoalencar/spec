// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Lexer = require("./lexer.bs.js");
var Lexing = require("rescript/lib/js/lexing.js");
var Parser = require("./parser.bs.js");
var Script = require("../script/script.bs.js");
var Source = require("../util/source.bs.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

function parse$p(name, lexbuf, start) {
  var init = lexbuf.lex_curr_p;
  lexbuf.lex_curr_p = {
    pos_fname: name,
    pos_lnum: init.pos_lnum,
    pos_bol: init.pos_bol,
    pos_cnum: init.pos_cnum
  };
  try {
    return Curry._2(start, Lexer.token, lexbuf);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Script.Syntax) {
      var region = exn._1;
      var region$p = Caml_obj.caml_notequal(region, Source.no_region) ? region : ({
            left: Lexer.convert_pos(lexbuf.lex_start_p),
            right: Lexer.convert_pos(lexbuf.lex_curr_p)
          });
      throw {
            RE_EXN_ID: Script.Syntax,
            _1: region$p,
            _2: exn._2,
            Error: new Error()
          };
    }
    throw exn;
  }
}

function parse(name, lexbuf, param) {
  switch (param) {
    case /* Module */0 :
        return parse$p(name, lexbuf, Parser.module1);
    case /* Script */1 :
        return parse$p(name, lexbuf, Parser.script);
    case /* Script1 */2 :
        return parse$p(name, lexbuf, Parser.script1);
    
  }
}

function string_to(start, s) {
  var lexbuf = Lexing.from_string(s);
  return parse("string", lexbuf, start);
}

function string_to_script(s) {
  return string_to(/* Script */1, s);
}

function string_to_module(s) {
  return string_to(/* Module */0, s)[1];
}

var Syntax = Script.Syntax;

exports.Syntax = Syntax;
exports.parse = parse;
exports.string_to_script = string_to_script;
exports.string_to_module = string_to_module;
/* Lexer Not a pure module */
