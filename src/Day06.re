let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = [|"1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"|];

module Part1 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  // let input = dummy_input;

  let points =
    input
    |> Array.map(str => {
         let coords =
           str |> Js.String.split(", ") |> Array.map(int_of_string);
         (coords[0], coords[1]);
       });

  // Js.log(points);

  let (minX, maxX, minY, maxY) =
    points
    |> Array.fold_left(
         ((minX, maxX, minY, maxY), (x, y)) =>
           (min(minX, x), max(maxX, x), min(minY, y), max(maxY, y)),
         (max_int, min_int, max_int, min_int),
       );

  // Js.log((minX, maxX, minY, maxY));

  let dist = ((x, y), (x', y')) => abs(x - x') + abs(y - y');

  type closestPoint =
    | Single(int, int)
    | Many(int)
    | None;

  let findClosestPoint = (points, (x, y)) => {
    points
    |> Js.Array.mapi((p, id) => (id, dist(p, (x, y))))
    |> Array.fold_left(
         (closestPoint, (id, distance)) =>
           switch (closestPoint) {
           | None => Single(id, distance)
           | Many(distance') =>
             if (distance' <= distance) {
               Many(distance');
             } else {
               Single(id, distance);
             }
           | Single(id', distance') =>
             if (distance == distance') {
               Many(distance);
             } else if (distance < distance') {
               Single(id, distance);
             } else {
               Single(id', distance');
             }
           },
         None,
       );
  };

  let pointIdToName = id => String.make(1, Char.chr(id + 65));
  let board = Array.make_matrix(maxX - minX + 1, maxY - minY + 1, "");
  let unboundedPoints = Belt.MutableSet.String.make();
  let histogram =
    points
    |> Js.Array.mapi((_, i) => pointIdToName(i))
    |> Array.map(id => (id, 0))
    |> Belt.MutableMap.String.fromArray;

  for (x in minX to maxX) {
    for (y in minY to maxY) {
      let closestPoint = findClosestPoint(points, (x, y));

      switch (closestPoint) {
      | None
      | Many(_) => board[x - minX][y - minY] = "."
      | Single(id, _) =>
        let pointName = pointIdToName(id);

        board[x - minX][y - minY] = pointName;

        Belt.MutableMap.String.update(histogram, pointName, prev =>
          switch (prev) {
          | None => Some(1)
          | Some(n) => Some(n + 1)
          }
        );

        if (x == minX || x == maxX || y == minY || y == maxY) {
          Belt.MutableSet.String.add(unboundedPoints, pointName);
        };
      };
    };
  };

  // let printBoard = board => {
  //   let output =
  //     board
  //     |> Array.map(Js.Array.joinWith(""))
  //     |> Js.Array.joinWith("\n")
  //   Js.log(output);
  // };
  // printBoard(board);

  // Js.log2(
  //   "unboundedPoints: ",
  //   Belt.MutableSet.String.toArray(unboundedPoints),
  // );
  // Js.log2("histogram: ", Belt.MutableMap.String.toArray(histogram));

  let maxArea =
    histogram
    |> Belt.MutableMap.String.toArray
    |> Js.Array.filter(((pointId, _)) =>
         !Belt.MutableSet.String.has(unboundedPoints, pointId)
       )
    |> Array.fold_left(
         ((_, area) as p1, (_, area') as p2) => area >= area' ? p1 : p2,
         ("", min_int),
       );

  Js.log2("Part1 result: ", maxArea);
};

module Part2 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  // let input = dummy_input;

  let points =
    input
    |> Array.map(str => {
         let coords =
           str |> Js.String.split(", ") |> Array.map(int_of_string);
         (coords[0], coords[1]);
       });

  let (minX, maxX, minY, maxY) =
    points
    |> Array.fold_left(
         ((minX, maxX, minY, maxY), (x, y)) =>
           (min(minX, x), max(maxX, x), min(minY, y), max(maxY, y)),
         (max_int, min_int, max_int, min_int),
       );

  let dist = ((x, y), (x', y')) => abs(x - x') + abs(y - y');
  let safeDistance = 10000;
  let safeSpotCounter = ref(0);

  for (x in minX to maxX) {
    for (y in minY to maxY) {
      let totalDistance =
        points |> Array.fold_left((acc, p) => acc + dist(p, (x, y)), 0);

      if (totalDistance < safeDistance) {
        safeSpotCounter := safeSpotCounter^ + 1;
      };
    };
  };

  Js.log2("Part2 result: ", safeSpotCounter^);
};