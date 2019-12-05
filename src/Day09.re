module Part1 = {
  let play = (nPlayers, lastMarble) => {
    let scores = Array.make(nPlayers, 0);

    let rec go = (marbles, marbleIndex, playerIndex, nextNumber) => {
      let nextPlayerIndex = (playerIndex + 1) mod nPlayers;

      if (nextNumber > lastMarble) {
        Array.fold_left(max, 0, scores);
      } else if (nextNumber mod 23 == 0) {
        let indexToRemove =
          (marbleIndex - 7 + Array.length(marbles)) mod Array.length(marbles);
        scores[playerIndex] =
          scores[playerIndex] + nextNumber + marbles[indexToRemove];

        let marbles' =
          Array.concat([
            Js.Array.slice(~start=0, ~end_=indexToRemove, marbles),
            Js.Array.sliceFrom(indexToRemove + 1, marbles),
          ]);
        let marbleIndex' = indexToRemove mod Array.length(marbles');

        go(marbles', marbleIndex', nextPlayerIndex, nextNumber + 1);
      } else {
        let marbleIndex' = (marbleIndex + 1) mod Array.length(marbles) + 1;
        let marbles' =
          Array.concat([
            Js.Array.slice(~start=0, ~end_=marbleIndex', marbles),
            [|nextNumber|],
            Js.Array.sliceFrom(marbleIndex', marbles),
          ]);

        go(marbles', marbleIndex', nextPlayerIndex, nextNumber + 1);
      };
    };

    go([|0|], 0, 0, 1);
  };
  // let result = play(9, 25); // high score is 32
  // let result = play(10, 1618); // high score is 8317
  // let result = play(13, 7999); // high score is 146373
  // let result = play(17, 1104); // high score is 2764
  // let result = play(21, 6111); // high score is 54718
  // let result = play(30, 5807); // high score is 37305
  let result = play(424, 71482);

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  // let result = play(424, 7148200);

  // Js.log2("Part2 result: ", result);
};