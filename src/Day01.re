let inputPath = "./src/Day01.input";

module Part1 = {
  let input =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String2.split(_, "\n")
    |> Array.map(int_of_string);

  let result = input |> Array.fold_left((+), 0);

  Js.log("Part1 result: " ++ string_of_int(result));
};

module Part2 = {
  module IntSet =
    Set.Make({
      type t = int;
      let compare = compare;
    });

  let changes =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String2.split(_, "\n")
    |> Array.map(int_of_string);

  let nChanges = Array.length(changes);

  let rec solve = (history, changeIndex, freq) =>
    if (IntSet.mem(freq, history)) {
      freq;
    } else {
      let nextIndex = (changeIndex + 1) mod nChanges;
      let currentChange = changes[changeIndex];
      solve(IntSet.add(freq, history), nextIndex, freq + currentChange);
    };

  let result = solve(IntSet.empty, 0, 0);

  Js.log("Part2 result: " ++ string_of_int(result));
};