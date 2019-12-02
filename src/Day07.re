let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = [|
  "Step C must be finished before step A can begin.",
  "Step C must be finished before step F can begin.",
  "Step A must be finished before step B can begin.",
  "Step A must be finished before step D can begin.",
  "Step B must be finished before step E can begin.",
  "Step D must be finished before step E can begin.",
  "Step F must be finished before step E can begin.",
|];

module Part1 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  // let input = dummy_input;

  let parseEdge = line => {
    let re = [%bs.re
      "/Step ([A-Z]) must be finished before step ([A-Z]) can begin./"
    ];

    let matches = Js.String.match(re, line) |> Js.Option.getExn;
    switch (matches) {
    | [|_, from, to_|] => (from, to_)
    | _ => failwith("invalid input")
    };
  };

  let edges = input |> Array.map(parseEdge);

  let topologicalSort = edges => {
    let vertices =
      edges
      |> Array.fold_left(
           (acc, (from, to_)) => {
             Belt.MutableSet.String.mergeMany(acc, [|from, to_|]);
             acc;
           },
           Belt.MutableSet.String.make(),
         );

    let rec go = (result, vertices, edges) =>
      if (Belt.MutableSet.String.isEmpty(vertices)) {
        result |> List.rev |> Array.of_list;
      } else {
        let froms =
          Belt.MutableSet.String.keep(vertices, v =>
            Js.Array.every(((_, to_)) => v != to_, edges)
          )
          |> Belt.MutableSet.String.toArray;

        Array.fast_sort(compare, froms);

        let nextFrom = froms[0];
        Belt.MutableSet.String.remove(vertices, nextFrom);
        let nextEdges =
          Js.Array.filter(((from, _)) => from != nextFrom, edges);

        go([nextFrom, ...result], vertices, nextEdges);
      };
    go([], vertices, edges);
  };

  let result = topologicalSort(edges) |> Js.Array.joinWith("")

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  // let input =
  //   inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  let input = dummy_input;

  Js.log2("Part2 result: ", "TBD");
};