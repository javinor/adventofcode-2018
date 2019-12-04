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

module Part1 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  // let input = dummy_input;

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

  let result = topologicalSort(edges) |> Js.Array.joinWith("");

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");
  // let input = dummy_input;
  let nWorkers = 5;
  let stepDurationBase = 60;

  let edges = input |> Array.map(parseEdge);
  let vertices =
    Belt.Set.String.(
      union(
        fromArray(edges |> Array.map(fst)),
        fromArray(edges |> Array.map(snd)),
      )
    );
  let tasksBlockedBy =
    edges
    |> Array.map(((from, to_)) => (from, [|to_|]))
    |> Array.fold_left(
         (acc, (blocker, blockedTasks)) =>
           Belt.Map.String.update(acc, blocker, arr =>
             switch (arr) {
             | None => Some(blockedTasks)
             | Some(otherBlockedTasks) =>
               Some(Array.append(blockedTasks, otherBlockedTasks))
             }
           ),
         Belt.Map.String.fromArray(
           vertices
           |> Belt.Set.String.toArray
           |> Array.map(task => (task, [||])),
         ),
       );

  let runScheduler =
      (nWorkers: int, tasksBlockedBy: Belt.Map.String.t(array(string))) => {
    let getFreeWorker = workers => {
      switch (Js.Array.findIndex(Js.Option.isNone, workers)) {
      | (-1) => None
      | i => Some(i)
      };
    };

    let getAssignableTask = (workers, tasksBlockedBy) => {
      let tasks =
        tasksBlockedBy
        |> Belt.Map.String.keysToArray
        |> Belt.Set.String.fromArray;

      let blockedTasks =
        tasksBlockedBy
        |> Belt.Map.String.valuesToArray
        |> Array.fold_left(
             (acc, values) =>
               Belt.Set.String.(union(acc, fromArray(values))),
             Belt.Set.String.empty,
           );

      let wipTasks =
        workers
        |> Js.Array.filter(Js.Option.isSome)
        |> Array.map(worker => worker |> Js.Option.getExn |> fst)
        |> Belt.Set.String.fromArray;

      let unblockedTasks = Belt.Set.String.diff(tasks, blockedTasks);
      let assignableTasks = Belt.Set.String.diff(unblockedTasks, wipTasks);
      Belt.Set.String.minimum(assignableTasks);
    };

    let rec go =
            (
              workers: array(option((string, int))),
              tasksBlockedBy: Belt.Map.String.t(array(string)),
              time: int,
            ) => {
      // Js.log3(time, workers, Belt.Map.String.toArray(tasksBlockedBy));

      let finishedTasks = workers
      |> Array.fold_left(
           (acc, w) =>
             switch (w) {
             | Some((task, ttl)) => ttl > time ? acc : [task, ...acc]
             | None => acc
             },
           [],
         ) |> Array.of_list
      let tasksBlockedBy' = Belt.Map.String.removeMany(tasksBlockedBy, finishedTasks)

      let workers' =
        workers
        |> Array.map(w =>
             switch (w) {
             | Some((task, ttl)) => ttl > time ? Some((task, ttl)) : None
             | None => None
             }
           );

      let finished =
        Belt.Map.String.isEmpty(tasksBlockedBy')
        && Js.Array.every(Js.Option.isNone, workers');

      switch (
        finished,
        getFreeWorker(workers'),
        getAssignableTask(workers', tasksBlockedBy'),
      ) {
      | (true, _, _) => time
      | (_, Some(workerIndex), Some(task)) =>
        let ttl = (task.[0] |> Char.code) - 64 + stepDurationBase; // base + letter index, e.g. A=1, B=2, etc.
        workers'[workerIndex] = Some((task, ttl + time));
        go(workers', tasksBlockedBy', time);
      | (_, Some(_), None)
      | (_, None, Some(_))
      | (_, None, None) => go(workers', tasksBlockedBy', time + 1)
      };
    };

    go(Array.make(nWorkers, None), tasksBlockedBy, 0);
  };

  let result = runScheduler(nWorkers, tasksBlockedBy);

  Js.log2("Part2 result: ", result);
};