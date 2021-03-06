// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var Path = require("path");
var $$Array = require("bs-platform/lib/js/array.js");
var $$String = require("bs-platform/lib/js/string.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");

var inputPath = "./src/" + (Path.parse("Day05.re").name + ".input");

function willReact(unit1, unit2) {
  if (unit1 !== unit2) {
    return $$String.capitalize(unit1) === $$String.capitalize(unit2);
  } else {
    return false;
  }
}

function triggerReactions(polymer) {
  var output = /* array */[];
  var input = polymer;
  while(true) {
    var len = input.length;
    if (len >= 3) {
      var x = Js_option.getExn(Caml_option.undefined_to_opt(input.shift()));
      var y = Js_option.getExn(Caml_option.undefined_to_opt(input.shift()));
      if (willReact(x, y)) {
        if (output.length !== 0) {
          var w = Js_option.getExn(Caml_option.undefined_to_opt(output.pop()));
          input.unshift(w);
        }
        continue ;
      } else {
        var z = Js_option.getExn(Caml_option.undefined_to_opt(input.shift()));
        if (willReact(y, z)) {
          input.unshift(x);
          continue ;
        } else {
          output.push(x);
          input.unshift(y, z);
          continue ;
        }
      }
    } else {
      switch (len) {
        case 0 : 
        case 1 : 
            return $$Array.append(output, input);
        case 2 : 
            var x$1 = input[0];
            var y$1 = input[1];
            var match = willReact(x$1, y$1);
            if (match) {
              return output;
            } else {
              return $$Array.append(output, input);
            }
        
      }
    }
  };
}

var input = Fs.readFileSync(inputPath, "utf8");

var result = triggerReactions(input.split("")).length;

console.log("Part1 result: " + String(result));

var Part1 = /* module */[
  /* willReact */willReact,
  /* triggerReactions */triggerReactions,
  /* input */input,
  /* result */result
];

var input$1 = Fs.readFileSync(inputPath, "utf8").split("");

var unitTypes = $$Array.fold_left((function (unitTypes, unit) {
        return Belt_SetString.add(unitTypes, $$String.capitalize(unit));
      }), Belt_SetString.empty, input$1);

var temp = Belt_MapString.toList(Belt_SetString.reduce(unitTypes, Belt_MapString.empty, (function (improvedPolymers, unitType) {
            var improvedLength = triggerReactions(input$1.filter((function (x) {
                        return $$String.capitalize(x) !== unitType;
                      }))).length;
            return Belt_MapString.set(improvedPolymers, unitType, improvedLength);
          })));

var result$1 = List.fold_left((function (param, param$1) {
        var length$prime = param$1[1];
        var length = param[1];
        var match = length < length$prime;
        if (match) {
          return /* tuple */[
                  param[0],
                  length
                ];
        } else {
          return /* tuple */[
                  param$1[0],
                  length$prime
                ];
        }
      }), /* tuple */[
      "",
      Pervasives.max_int
    ], temp);

console.log("Part2 result: " + String(result$1[1]));

console.log(result$1);

var Part2 = /* module */[
  /* input */input$1,
  /* unitTypes */unitTypes,
  /* temp */temp,
  /* result */result$1
];

var dummy_input = "dabAcCaCBAcCcaDA";

exports.inputPath = inputPath;
exports.dummy_input = dummy_input;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* inputPath Not a pure module */
