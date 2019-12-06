module CyclicDLList = {
  type element('a) = {
    value: 'a,
    mutable next: option(element('a)),
    mutable prev: option(element('a)),
  };

  let singleton = (value: 'a) => {
    let el = {value, next: None, prev: None};
    el.next = Some(el);
    el.prev = Some(el);
    el;
  };

  let insertAfter = (value, el) => {
    let newEl = {value, prev: Some(el), next: el.next};
    switch (el.next) {
    | None => ()
    | Some(oldNext) => oldNext.prev = Some(newEl)
    };
    el.next = Some(newEl);
    newEl;
  };

  let remove = el => {
    let {prev, next} = el;

    switch (prev) {
    | None => ()
    | Some(prev) => prev.next = next
    };
    switch (next) {
    | None => ()
    | Some(next) => next.prev = prev
    };

    el.prev = None;
    el.next = None;
  };

  let rec prevN = (el, n) =>
    n == 0 ? el : prevN(Js.Option.getExn(el.prev), n - 1);
};

let play = (nPlayers, lastMarble) => {
  let scores = Array.make(nPlayers, 0.);

  let rec go =
          (
            currentMarble: CyclicDLList.element(int),
            currentPlayer: int,
            nextMarbleValue: int,
          ) => {
    let nextPlayer = (currentPlayer + 1) mod nPlayers;

    if (nextMarbleValue > lastMarble) {
      Array.fold_left(max, 0., scores);
    } else if (nextMarbleValue mod 23 == 0) {
      let marbleToRemove = CyclicDLList.prevN(currentMarble, 7);
      let nextCurrentMarble = marbleToRemove.next |> Js.Option.getExn;

      scores[currentPlayer] =
        scores[currentPlayer]
        +. float_of_int(nextMarbleValue + marbleToRemove.value);

      CyclicDLList.remove(marbleToRemove);

      go(nextCurrentMarble, nextPlayer, nextMarbleValue + 1);
    } else {
      let tempMarble = currentMarble.next |> Js.Option.getExn;
      let nextCurrentMarble =
        CyclicDLList.insertAfter(nextMarbleValue, tempMarble);

      go(nextCurrentMarble, nextPlayer, nextMarbleValue + 1);
    };
  };

  go(CyclicDLList.singleton(0), 0, 1);
};

module Part1 = {
  // let result = play(9, 25); // high score is 32
  // let result = play(10, 1618); // high score is 8317
  // let result = play(13, 7999); // high score is 146373
  // let result = play(17, 1104); // high score is 2764
  // let result = play(21, 6111); // high score is 54718
  // let result = play(30, 5807); // high score is 37305
  let result = play(424, 71482);

  Js.log2("Part1 result: ", result);
};

module Part2 = {
  let result = play(424, 7148200);

  Js.log2("Part2 result: ", result);
};