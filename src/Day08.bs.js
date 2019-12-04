// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var Path = require("path");
var $$Array = require("bs-platform/lib/js/array.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var inputPath = "./src/" + (Path.parse("Day08.re").name + ".input");

var input = Fs.readFileSync(inputPath, "utf8");

function parseTree(numbers) {
  var parseNode = function (numbers) {
    if (numbers) {
      var match = numbers[1];
      if (match) {
        var children = /* [] */0;
        var numbers$prime = match[1];
        for(var _for = 0 ,_for_finish = numbers[0] - 1 | 0; _for <= _for_finish; ++_for){
          var match$1 = parseNode(numbers$prime);
          children = /* :: */[
            match$1[0],
            children
          ];
          numbers$prime = match$1[1];
        }
        children = List.rev(children);
        var match$2 = Belt_Option.getExn(Belt_List.splitAt(numbers$prime, match[0]));
        return /* tuple */[
                /* Node */[
                  children,
                  match$2[0]
                ],
                match$2[1]
              ];
      } else {
        return Pervasives.failwith("illegal input - expected at least two numbers to parse tree");
      }
    } else {
      return Pervasives.failwith("illegal input - expected at least two numbers to parse tree");
    }
  };
  return parseNode(numbers)[0];
}

function sumTree(param) {
  var sumData = List.fold_left((function (prim, prim$1) {
          return prim + prim$1 | 0;
        }), 0, param[1]);
  var sumChildren = List.fold_left((function (prim, prim$1) {
          return prim + prim$1 | 0;
        }), 0, List.map(sumTree, param[0]));
  return sumData + sumChildren | 0;
}

var numbers = $$Array.to_list($$Array.map(Caml_format.caml_int_of_string, input.split(" ")));

var result = sumTree(parseTree(numbers));

console.log("Part1 result: ", result);

var Part1 = /* module */[
  /* sumTree */sumTree,
  /* numbers */numbers,
  /* result */result
];

function complexSumTree(param) {
  var data = param[1];
  var children = $$Array.of_list(param[0]);
  if (children.length === 0) {
    return List.fold_left((function (prim, prim$1) {
                  return prim + prim$1 | 0;
                }), 0, data);
  } else {
    var values = $$Array.map(complexSumTree, children);
    return List.fold_left((function (acc, datum) {
                  var index = datum - 1 | 0;
                  var match = 0 <= index && index < values.length;
                  var value = match ? Caml_array.caml_array_get(values, index) : 0;
                  return acc + value | 0;
                }), 0, data);
  }
}

var numbers$1 = $$Array.to_list($$Array.map(Caml_format.caml_int_of_string, input.split(" ")));

var result$1 = complexSumTree(parseTree(numbers$1));

console.log("Part2 result: ", result$1);

var Part2 = /* module */[
  /* complexSumTree */complexSumTree,
  /* numbers */numbers$1,
  /* result */result$1
];

var dummy_input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";

exports.inputPath = inputPath;
exports.input = input;
exports.dummy_input = dummy_input;
exports.parseTree = parseTree;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* inputPath Not a pure module */
