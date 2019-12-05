// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");

function play(nPlayers, lastMarble) {
  var scores = Caml_array.caml_make_vect(nPlayers, 0);
  var _marbles = /* array */[0];
  var _marbleIndex = 0;
  var _playerIndex = 0;
  var _nextNumber = 1;
  while(true) {
    var nextNumber = _nextNumber;
    var playerIndex = _playerIndex;
    var marbleIndex = _marbleIndex;
    var marbles = _marbles;
    var nextPlayerIndex = Caml_int32.mod_(playerIndex + 1 | 0, nPlayers);
    if (nextNumber > lastMarble) {
      return $$Array.fold_left(Caml_obj.caml_max, 0, scores);
    } else if (nextNumber % 23 === 0) {
      var indexToRemove = Caml_int32.mod_((marbleIndex - 7 | 0) + marbles.length | 0, marbles.length);
      Caml_array.caml_array_set(scores, playerIndex, (Caml_array.caml_array_get(scores, playerIndex) + nextNumber | 0) + Caml_array.caml_array_get(marbles, indexToRemove) | 0);
      var marbles$prime = $$Array.concat(/* :: */[
            marbles.slice(0, indexToRemove),
            /* :: */[
              marbles.slice(indexToRemove + 1 | 0),
              /* [] */0
            ]
          ]);
      var marbleIndex$prime = Caml_int32.mod_(indexToRemove, marbles$prime.length);
      _nextNumber = nextNumber + 1 | 0;
      _playerIndex = nextPlayerIndex;
      _marbleIndex = marbleIndex$prime;
      _marbles = marbles$prime;
      continue ;
    } else {
      var marbleIndex$prime$1 = Caml_int32.mod_(marbleIndex + 1 | 0, marbles.length) + 1 | 0;
      var marbles$prime$1 = $$Array.concat(/* :: */[
            marbles.slice(0, marbleIndex$prime$1),
            /* :: */[
              /* array */[nextNumber],
              /* :: */[
                marbles.slice(marbleIndex$prime$1),
                /* [] */0
              ]
            ]
          ]);
      _nextNumber = nextNumber + 1 | 0;
      _playerIndex = nextPlayerIndex;
      _marbleIndex = marbleIndex$prime$1;
      _marbles = marbles$prime$1;
      continue ;
    }
  };
}

var result = play(424, 71482);

console.log("Part1 result: ", result);

var Part1 = /* module */[
  /* play */play,
  /* result */result
];

var Part2 = /* module */[];

exports.Part1 = Part1;
exports.Part2 = Part2;
/* result Not a pure module */
