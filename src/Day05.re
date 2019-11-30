let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = "dabAcCaCBAcCcaDA";

module Part1 = {
  let willReact = (unit1, unit2) =>
    unit1 != unit2 && String.capitalize(unit1) == String.capitalize(unit2);

  let triggerReactions = polymer => {
    let rec go = (output, input) => {
      switch (input) {
      | [||]
      | [|_|] => Array.append(output, input)
      | [|x, y|] => willReact(x, y) ? output : Array.append(output, input)
      | _ =>
        let x = Js.Array.shift(input) |> Js.Option.getExn;
        let y = Js.Array.shift(input) |> Js.Option.getExn;

        if (willReact(x, y)) {
          if (Array.length(output) > 0) {
            let w = Js.Array.pop(output) |> Js.Option.getExn;
            let _ = Js.Array.unshift(w, input);
            ();
          };
          go(output, input);
        } else {
          let z = Js.Array.shift(input) |> Js.Option.getExn;

          if (willReact(y, z)) {
            let _ = Js.Array.unshift(x, input);
            go(output, input);
          } else {
            let _ = Js.Array.push(x, output);
            let _ = Js.Array.unshiftMany([|y, z|], input);
            go(output, input);
          };
        };
      };
    };

    go([||], polymer);
  };

  let input = inputPath |> Node.Fs.readFileAsUtf8Sync;

  let result =
    input |> Js.String.split("") |> triggerReactions |> Array.length;

  Js.log("Part1 result: " ++ string_of_int(result));
};

module Part2 = {
  let input = inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("");
  // let input = dummy_input |> Js.String.split("");

  let unitTypes =
    input
    |> Array.fold_left(
         (unitTypes, unit) =>
           Belt.Set.String.add(unitTypes, String.capitalize(unit)),
         Belt.Set.String.empty,
       );

  // Js.log(Belt.Set.String.toArray(unitTypes));

  let temp =
    unitTypes
    |> Belt.Set.String.reduce(
         _,
         Belt.Map.String.empty,
         (improvedPolymers, unitType) => {
           let improvedLength =
             input
             |> Js.Array.filter(x => String.capitalize(x) != unitType)
             |> Part1.triggerReactions
             |> Array.length;
           Belt.Map.String.set(improvedPolymers, unitType, improvedLength);
         },
       )
    |> Belt.Map.String.toList;

  // Js.log(Array.of_list(temp));

  let result =
    temp
    |> List.fold_left(
         ((unitType, length), (unitType', length')) =>
           length < length' ? (unitType, length) : (unitType', length'),
         ("", max_int),
       );

  Js.log("Part2 result: " ++ string_of_int(snd(result)));
  Js.log(result);
};