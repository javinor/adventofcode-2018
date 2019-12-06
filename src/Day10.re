let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";
let input = inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");

let dummy_input = [|
  "position=< 9,  1> velocity=< 0,  2>",
  "position=< 7,  0> velocity=<-1,  0>",
  "position=< 3, -2> velocity=<-1,  1>",
  "position=< 6, 10> velocity=<-2, -1>",
  "position=< 2, -4> velocity=< 2,  2>",
  "position=<-6, 10> velocity=< 2, -2>",
  "position=< 1,  8> velocity=< 1, -1>",
  "position=< 1,  7> velocity=< 1,  0>",
  "position=<-3, 11> velocity=< 1, -2>",
  "position=< 7,  6> velocity=<-1, -1>",
  "position=<-2,  3> velocity=< 1,  0>",
  "position=<-4,  3> velocity=< 2,  0>",
  "position=<10, -3> velocity=<-1,  1>",
  "position=< 5, 11> velocity=< 1, -2>",
  "position=< 4,  7> velocity=< 0, -1>",
  "position=< 8, -2> velocity=< 0,  1>",
  "position=<15,  0> velocity=<-2,  0>",
  "position=< 1,  6> velocity=< 1,  0>",
  "position=< 8,  9> velocity=< 0, -1>",
  "position=< 3,  3> velocity=<-1,  1>",
  "position=< 0,  5> velocity=< 0, -1>",
  "position=<-2,  2> velocity=< 2,  0>",
  "position=< 5, -2> velocity=< 1,  2>",
  "position=< 1,  4> velocity=< 2,  1>",
  "position=<-2,  7> velocity=< 2, -2>",
  "position=< 3,  6> velocity=<-1, -1>",
  "position=< 5,  0> velocity=< 1,  0>",
  "position=<-6,  0> velocity=< 2,  0>",
  "position=< 5,  9> velocity=< 1, -2>",
  "position=<14,  7> velocity=<-2,  0>",
  "position=<-3,  6> velocity=< 2, -1>",
|];

let parseLine = line => {
  let re = [%bs.re "/^position=<(.+), (.+)> velocity=<(.+), (.+)>$/"];

  switch (
    Js.String.match(re, line)
    |> Js.Option.getExn
    |> Js.Array.sliceFrom(1)
    |> Array.map(Js.String.trim)
    |> Array.map(int_of_string)
  ) {
  | exception e =>
    Js.log2("exception was raised while parsing input: " ++ line, e);
    raise(e);
  | [|px, py, vx, vy|] => ((px, py), (vx, vy))
  | _ => failwith("failed to parse input: " ++ line)
  };
};

let printPoints = points => {
  let (minX, maxX, minY, maxY) =
    points
    |> Array.fold_left(
         ((minX, maxX, minY, maxY), ((x, y), _)) =>
           (min(minX, x), max(maxX, x), min(minY, y), max(maxY, y)),
         (max_int, min_int, max_int, min_int),
       );

  // Js.log((minX, maxX, minY, maxY));

  if (maxX - minX <= 100 && maxY - minY <= 20) {
    let nRows = maxY - minY + 1;
    let nCols = maxX - minX + 1;
    let grid = Array.make_matrix(nRows, nCols, ".");

    Array.iter(
      (((x, y), _)) => {
        let row = y - minY;
        let col = x - minX;
        grid[row][col] = "#";
      },
      points,
    );

    grid
    |> Array.map(row => Js.Array.joinWith("", row))
    |> Js.Array.joinWith("\n")
    |> Js.log;

    true;
  } else {
    false;
  };
};

let tick = points => {
  points
  |> Array.map((((x, y), (vx, vy))) => ((x + vx, y + vy), (vx, vy)));
};

module Part1And2 = {
  // let input = dummy_input;
  let points = input |> Array.map(parseLine);
  printPoints(points);

  let points' = ref(points);
  for (t in 1 to 50000) {
    points' := tick(points'^);
    let printTime = printPoints(points'^);
    if (printTime) {
      Js.log(t);
    };
  };
};