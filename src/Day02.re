let inputPath = "./src/Day02.input";

module Part1 = {
  module StringMap = Map.Make(String);
  type checksumType =
    | ExactlyTwo
    | ExactlyThree
    | BothTwoAndThree
    | None;

  let calculateChecksumType = str => {
    let letters = str |> Js.String.split("");

    let letterHistogram =
      letters
      |> Array.fold_left(
           (hist, letter) => {
             let freq =
               StringMap.mem(letter, hist)
                 ? StringMap.find(letter, hist) + 1 : 1;

             StringMap.add(letter, freq, hist);
           },
           StringMap.empty,
         );

    let result =
      StringMap.fold(
        (_, freq, checksumType) =>
          switch (freq, checksumType) {
          | (2, ExactlyThree)
          | (3, ExactlyTwo) => BothTwoAndThree
          | (2, None) => ExactlyTwo
          | (3, None) => ExactlyThree
          | _ => checksumType
          },
        letterHistogram,
        None,
      );

    result;
  };

  let (nTwos, nThrees) =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String2.split(_, "\n")
    |> Array.map(calculateChecksumType)
    |> Array.fold_left(
         ((nTwos, nThrees), csType) =>
           switch (csType) {
           | BothTwoAndThree => (nTwos + 1, nThrees + 1)
           | ExactlyThree => (nTwos, nThrees + 1)
           | ExactlyTwo => (nTwos + 1, nThrees)
           | None => (nTwos, nThrees)
           },
         (0, 0),
       );

  Js.log("Part1 result: " ++ string_of_int(nTwos * nThrees));
};

module Part2 = {
  let rec differByOne = (s1, s2) =>
    if (s1 == "" || s2 == "") {
      false;
    } else {
      let length = String.length(s1);
      let tail1 = String.sub(s1, 1, length - 1);
      let tail2 = String.sub(s2, 1, length - 1);

      if (s1.[0] != s2.[0]) {
        tail1 == tail2;
      } else {
        differByOne(tail1, tail2);
      };
    };

  let rec findPair:
    ((string, string) => bool, list(string)) => (string, string) =
    (p, ids) => {
      switch (ids) {
      | []
      | [_] => failwith("no pair found")
      | [id, ...ids] =>
        switch (List.find(differByOne(id), ids)) {
        | id' => (id, id')
        | exception Not_found => findPair(p, ids)
        }
      };
    };

  let ids =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String2.split(_, "\n") |> Array.to_list

  let (id1,id2) = findPair(differByOne, ids);
  
  let result = ref("")
  for (ii in 0 to String.length(id1) - 1) {
    if (id1.[ii] == id2.[ii]) {
      result := result^ ++ String.sub(id1, ii, 1)
    }
  }

  Js.log("Part2 result: " ++ result^);
};