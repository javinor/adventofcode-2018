let inputPath = "./src/" ++ Node.Path.parse(__FILE__)##name ++ ".input";

let dummy_input = [|
  "[1518-11-01 00:00] Guard #10 begins shift",
  "[1518-11-01 00:05] falls asleep",
  "[1518-11-01 00:25] wakes up",
  "[1518-11-01 00:30] falls asleep",
  "[1518-11-01 00:55] wakes up",
  "[1518-11-01 23:58] Guard #99 begins shift",
  "[1518-11-02 00:40] falls asleep",
  "[1518-11-02 00:50] wakes up",
  "[1518-11-03 00:05] Guard #10 begins shift",
  "[1518-11-03 00:24] falls asleep",
  "[1518-11-03 00:29] wakes up",
  "[1518-11-04 00:02] Guard #99 begins shift",
  "[1518-11-04 00:36] falls asleep",
  "[1518-11-04 00:46] wakes up",
  "[1518-11-05 00:03] Guard #99 begins shift",
  "[1518-11-05 00:45] falls asleep",
  "[1518-11-05 00:55] wakes up",
|];

module IntMap =
  Map.Make({
    type t = int;
    let compare = compare;
  });

let int_of_bool = b => b ? 1 : 0;

type logEvent =
  | BeginsShift(int)
  | FallsAsleep(int)
  | WakesUp(int);

let bumpDate = date => {
  let epoch = Js.Date.fromString("1518-" ++ date) |> Js.Date.valueOf;
  let oneDay_f = 24. *. 60. *. 60. *. 1000.;
  let d = Js.Date.fromFloat(epoch +. oneDay_f);

  let month_f = Js.Date.getMonth(d) +. 1.;
  let month = (month_f < 10. ? "0" : "") ++ Js.Float.toString(month_f);
  let day_f = Js.Date.getDate(d);
  let day = (day_f < 10. ? "0" : "") ++ Js.Float.toString(day_f);

  month ++ "-" ++ day;
};

let parseLogLine = line => {
  let re = [%bs.re "/\[1518-(\d\d-\d\d) (\d\d):(\d\d)] (.*$)/"];

  let matches = Js.String.match(re, line) |> Js.Option.getExn;
  switch (matches) {
  | [|_, date, hour, minute, message|] =>
    let event =
      switch (message) {
      | "wakes up" => WakesUp(int_of_string(minute))
      | "falls asleep" => FallsAsleep(int_of_string(minute))
      | _ =>
        let re' = [%bs.re "/\\d+/"];
        let guardId =
          Js.String.match(re', message)
          |> Js.Option.getExn
          |> Js.Array.unsafe_get(_, 0)
          |> int_of_string;

        BeginsShift(guardId);
      };

    let date = hour != "23" ? date : bumpDate(date);

    (date, event);

  | _ => failwith("invalid input")
  };
};

let mkZzzRecords = input => {
  input
  |> Js.Array.sortInPlace
  |> Js.Array.map(parseLogLine)
  |> Array.to_list
  |> List.fold_left(
       (zzzRecords, (date, event)) =>
         switch (event) {
         | BeginsShift(guardId) =>
           let asleepAtMinute = Array.make(60, false);
           let newRecord = (date, guardId, asleepAtMinute);
           [newRecord, ...zzzRecords];
         | WakesUp(minute) =>
           let (_, _, asleepAtMinute) = List.hd(zzzRecords);
           Array.fill(asleepAtMinute, minute, 60 - minute, false);
           zzzRecords;
         | FallsAsleep(minute) =>
           let (_, _, asleepAtMinute) = List.hd(zzzRecords);
           Array.fill(asleepAtMinute, minute, 60 - minute, true);
           zzzRecords;
         },
       [],
     );
};

let printZzzRecords = zzzRecords =>
  List.iter(
    ((date, guardId, asleepAtMinute)) => {
      let asleepAtMinute' =
        asleepAtMinute
        |> Array.map(isAsleep => isAsleep ? "#" : ".")
        |> Js.Array.joinWith("");
      Js.log(
        date ++ " #" ++ string_of_int(guardId) ++ " " ++ asleepAtMinute',
      );
    },
    zzzRecords,
  );

module Part1 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");

  let zzzRecords = mkZzzRecords(input);

  let (topSleeper, topDuration) =
    zzzRecords
    |> List.fold_left(
         (sleepDurations, (_, guardId, asleepAtMinute)) => {
           let duration =
             asleepAtMinute
             |> Array.map(int_of_bool)
             |> Array.fold_left((+), 0);
           IntMap.merge(
             (_, dur1, dur2) => {
               let dur1' = Js.Option.getWithDefault(0, dur1);
               let dur2' = Js.Option.getWithDefault(0, dur2);
               Some(dur1' + dur2');
             },
             sleepDurations,
             IntMap.singleton(guardId, duration),
           );
         },
         IntMap.empty,
       )
    |> IntMap.bindings
    |> List.fold_left(
         ((gid, dur), (gid', dur')) =>
           if (dur > dur') {
             (gid, dur);
           } else {
             (gid', dur');
           },
         (0, 0),
       );

  let napsAtMinute =
    zzzRecords
    |> List.filter(((_, guardId, _)) => guardId == topSleeper)
    |> List.fold_left(
         (acc, (_, _, asleepAtMinute)) =>
           acc |> Js.Array.mapi((n, i) => n + int_of_bool(asleepAtMinute[i])),
         Array.make(60, 0),
       );

  let topNaps = Array.fold_left(max, 0, napsAtMinute);
  let sleepiestMinute =
    Js.Array.findIndex(naps => naps == topNaps, napsAtMinute);

  Js.log("topSleeper: " ++ string_of_int(topSleeper));
  Js.log("sleepiestMinute: " ++ string_of_int(sleepiestMinute));
  Js.log("Part1 result: " ++ string_of_int(topSleeper * sleepiestMinute));
};

module Part2 = {
  let input =
    inputPath |> Node.Fs.readFileAsUtf8Sync |> Js.String.split("\n");

  let zzzRecords = mkZzzRecords(input);

  let napCountAtMinute =
    zzzRecords
    |> List.fold_left(
         (napCounts, (_, guardId, asleepAtMinute)) => {
           let napCount = asleepAtMinute |> Array.map(int_of_bool);

           IntMap.merge(
             (_, nNaps1, nNaps2) => {
               let nNaps1' =
                 Js.Option.getWithDefault(Array.make(60, 0), nNaps1);
               let nNaps2' =
                 Js.Option.getWithDefault(Array.make(60, 0), nNaps2);

               Some(Belt.Array.zipBy(nNaps1', nNaps2', (+)));
             },
             napCounts,
             IntMap.singleton(guardId, napCount),
           );
         },
         IntMap.empty,
       );

  let (topSleeper, (sleepiestMinute, nNaps)) =
    napCountAtMinute
    |> IntMap.map(napCount => {
         let topNaps = Array.fold_left(max, 0, napCount);
         let sleepiestMinute =
           Js.Array.findIndex(naps => naps == topNaps, napCount);
         (sleepiestMinute, topNaps);
       })
    |> IntMap.bindings
    |> List.fold_left(
         (
           (gid, (sleepiestMinute, nNaps)),
           (gid', (sleepiestMinute', nNaps')),
         ) =>
           if (nNaps > nNaps') {
             (gid, (sleepiestMinute, nNaps));
           } else {
             (gid', (sleepiestMinute', nNaps'));
           },
         (0, (0, 0)),
       );

  Js.log("topSleeper: " ++ string_of_int(topSleeper));
  Js.log("sleepiestMinute: " ++ string_of_int(sleepiestMinute));
  Js.log("Part2 result: " ++ string_of_int(topSleeper * sleepiestMinute));
};