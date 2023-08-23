type rec puzzle = {
  crates: array<array<string>>,
  moves: array<move>,
}
and move = {
  from: int,
  to: int,
  move: int,
}

let parseMove = input =>
  switch input
  ->String.match(%re("/^move ([0-9]+?) from ([0-9]+?) to ([0-9]+?)$/"))
  ->Option.map(matches => matches->Array.filterMap(v => v->Int.fromString)) {
  | Some([move, from, to]) => Some({move, from: from - 1, to: to - 1})
  | _ => None
  }

let parseCrates = input =>
  switch input->String.split("\n")->Array.toReversed->List.fromArray {
  | list{} => []
  | list{heading, ...stacks} => {
      let charIndexes =
        heading
        ->String.split("")
        ->Array.mapWithIndex((char, index) => {
          switch Int.fromString(char) {
          | Some(_) => Some(index)
          | None => None
          }
        })
        ->Array.filterMap(v => v)
      let stacks = List.toArray(stacks)
      charIndexes->Array.map(charIndex => {
        stacks
        ->Array.map(stack => stack->String.charAt(charIndex))
        ->Array.filter(char => char->String.trim != "")
      })
    }
  }

let parseInput = input =>
  switch input->String.split("\n\n") {
  | [crates, moves] =>
    Some({
      crates: crates->parseCrates,
      moves: moves->String.split("\n")->Array.filterMap(parseMove),
    })
  | _ => None
  }

let part1 =
  Utils.readInput("Day5.txt")
  ->parseInput
  ->Option.map(({crates, moves}) => {
    moves
    ->Array.reduce(crates, (result, {move, from, to}) => {
      let toStack = result->Array.get(to)->Option.getWithDefault([])
      let fromStack = result->Array.get(from)->Option.getWithDefault([])
      let moveApart = fromStack->Js.Array2.removeFromInPlace(~pos=-move)
      result->Array.set(to, toStack->Array.concat(moveApart->Array.toReversed))
      result
    })
    ->Belt.Array.keepMap(crate => crate->Array.get(Array.length(crate) - 1))
    ->Array.joinWith("")
  })

let part2 =
  Utils.readInput("Day5.txt")
  ->parseInput
  ->Option.map(({crates, moves}) => {
    moves
    ->Array.reduce(crates, (result, {move, from, to}) => {
      let toStack = result->Array.get(to)->Option.getWithDefault([])
      let fromStack = result->Array.get(from)->Option.getWithDefault([])
      let moveApart = fromStack->Js.Array2.removeFromInPlace(~pos=-move)
      result->Array.set(to, toStack->Array.concat(moveApart))
      result
    })
    ->Array.filterMap(crate => crate->Array.get(Array.length(crate) - 1))
    ->Array.joinWith("")
  })

Console.log({"part1": part1, "part2": part2})
