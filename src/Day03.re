let inputPath = "./src/Day03.input";

module Part1 = {
  type claim = {
    id: int,
    top: int,
    left: int,
    width: int,
    height: int,
  };

  let parseClaim = str => {
    let re = Js.Re.fromStringWithFlags("\\d+", ~flags="g");
    let matches =
      Js.String.match(re, str)
      |> Js.Option.getExn
      |> Array.map(int_of_string);
    switch (matches) {
    | [|id, left, top, width, height|] => {id, top, left, width, height}
    | _ => failwith("invalid input")
    };
  };

  let claims =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String2.split(_, "\n")
    |> Array.map(parseClaim);

  let result =
    claims
    |> Array.fold_left(
         (fabric, claim) => {
           for (dx in 0 to claim.width - 1) {
             for (dy in 0 to claim.height - 1) {
               fabric[claim.left + dx][claim.top + dy] =
                 fabric[claim.left + dx][claim.top + dy] + 1;
             };
           };

           fabric;
         },
         Array.make_matrix(1000, 1000, 0),
       )
    |> Array.fold_left(
         (acc, arr) =>
           arr |> Array.map(n => n > 1 ? 1 : 0) |> Array.fold_left((+), acc),
         0,
       );

  Js.log("Part1 result: " ++ string_of_int(result));
};

module Part2 = {
  type claim = {
    id: int,
    top: int,
    left: int,
    width: int,
    height: int,
  };

  let parseClaim = str => {
    let re = Js.Re.fromStringWithFlags("\\d+", ~flags="g");
    let matches =
      Js.String.match(re, str)
      |> Js.Option.getExn
      |> Array.map(int_of_string);
    switch (matches) {
    | [|id, left, top, width, height|] => {id, top, left, width, height}
    | _ => failwith("invalid input")
    };
  };

  let claims =
    inputPath
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String2.split(_, "\n")
    |> Array.map(parseClaim);

  type fabricClaims =
    | None
    | Single(int)
    | Multi(list(int));

  let fabric =
    claims
    |> Array.fold_left(
         (fabric, claim) => {
           for (dx in 0 to claim.width - 1) {
             for (dy in 0 to claim.height - 1) {
               fabric[claim.left + dx][claim.top + dy] = (
                 switch (fabric[claim.left + dx][claim.top + dy]) {
                 | None => Single(claim.id)
                 | Single(id) => Multi([claim.id, id])
                 | Multi(ids) => Multi([claim.id, ...ids])
                 }
               );
             };
           };

           fabric;
         },
         Array.make_matrix(1000, 1000, None),
       );

  module IntSet =
    Set.Make({
      type t = int;
      let compare = compare;
    });

  let badIds =
    fabric
    |> Array.fold_left(
         (badIds, arr) =>
           Array.fold_left(
             (badIds, claims) =>
               switch (claims) {
               | None | Single(_) => badIds
               | Multi(ids) => IntSet.union(badIds, IntSet.of_list(ids))
               },
             badIds,
             arr,
           ),
         IntSet.empty,
       );

  let result =
    claims
    |> Array.map(c => c.id)
    |> Js.Array.filter(id => !IntSet.mem(id, badIds));

  Js.log("Part2 result: " ++ string_of_int(result[0]));
};