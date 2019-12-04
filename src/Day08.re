let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync;
let dummy_input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";

type node =
  | Node(list(node), list(int));

let parseTree = numbers => {
  let rec parseNode = numbers => {
    switch (numbers) {
    | []
    | [_] =>
      failwith("illegal input - expected at least two numbers to parse tree")
    | [nChildren, nData, ...rest] =>
      let children = ref([]);
      let numbers' = ref(rest);

      for (_ in 0 to nChildren - 1) {
        let (node, remainingNumbers) = parseNode(numbers'^);
        children := [node, ...children^];
        numbers' := remainingNumbers;
      };
      children := List.rev(children^);

      let (data, remainingNumbers) =
        Belt.List.splitAt(numbers'^, nData) |> Belt.Option.getExn;

      (Node(children^, data), remainingNumbers);
    };
  };

  parseNode(numbers) |> fst;
};

module Part1 = {
  let rec sumTree = (Node(children, data)) => {
    let sumData = data |> List.fold_left((+), 0);
    let sumChildren =
      children |> List.map(sumTree) |> List.fold_left((+), 0);
    sumData + sumChildren;
  };

  let numbers =
    input
    |> Js.String.split(" ")
    |> Array.map(int_of_string)
    |> Array.to_list;

  let result = numbers |> parseTree |> sumTree;

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  let rec complexSumTree = (Node(children, data)) => {
    let children = Array.of_list(children);

    if (Array.length(children) == 0) {
      data |> List.fold_left((+), 0);
    } else {
      let values = children |> Array.map(complexSumTree);
      data
      |> List.fold_left(
           (acc, datum) => {
             let index = datum - 1;
             let value =
               0 <= index && index < Array.length(values) ? values[index] : 0;
             acc + value;
           },
           0,
         );
    };
  };

  let numbers =
    input
    |> Js.String.split(" ")
    |> Array.map(int_of_string)
    |> Array.to_list;

  let result = numbers |> parseTree |> complexSumTree;

  Js.log2("Part2 result: ", result);
};