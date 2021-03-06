// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Set = require("bs-platform/lib/js/set.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var inputPath = "./src/Day01.input";

var input = $$Array.map(Caml_format.caml_int_of_string, Fs.readFileSync(inputPath, "utf8").split("\n"));

var result = $$Array.fold_left((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }), 0, input);

console.log("Part1 result: " + String(result));

var Part1 = /* module */[
  /* input */input,
  /* result */result
];

var compare = Caml_obj.caml_compare;

var IntSet = $$Set.Make(/* module */[/* compare */compare]);

var changes = $$Array.map(Caml_format.caml_int_of_string, Fs.readFileSync(inputPath, "utf8").split("\n"));

var nChanges = changes.length;

function solve(_history, _changeIndex, _freq) {
  while(true) {
    var freq = _freq;
    var changeIndex = _changeIndex;
    var history = _history;
    if (Curry._2(IntSet[/* mem */2], freq, history)) {
      return freq;
    } else {
      var nextIndex = Caml_int32.mod_(changeIndex + 1 | 0, nChanges);
      var currentChange = Caml_array.caml_array_get(changes, changeIndex);
      _freq = freq + currentChange | 0;
      _changeIndex = nextIndex;
      _history = Curry._2(IntSet[/* add */3], freq, history);
      continue ;
    }
  };
}

var result$1 = solve(IntSet[/* empty */0], 0, 0);

console.log("Part2 result: " + String(result$1));

var Part2 = /* module */[
  /* IntSet */IntSet,
  /* changes */changes,
  /* nChanges */nChanges,
  /* solve */solve,
  /* result */result$1
];

exports.inputPath = inputPath;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* input Not a pure module */
