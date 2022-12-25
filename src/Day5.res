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
  ->Js.String2.match_(%re("/^move ([0-9]+?) from ([0-9]+?) to ([0-9]+?)$/"))
  ->Option.map(matches => matches->Array.keepMap(v => v)->Array.keepMap(Int.fromString)) {
  | Some([move, from, to]) => Some({move, from: from - 1, to: to - 1})
  | _ => None
  }

let parseCrates = input =>
  switch input->Js.String2.split("\n")->Array.reverse->List.fromArray {
  | list{} => []
  | list{heading, ...stacks} => {
      let charIndexes =
        heading
        ->Js.String2.split("")
        ->Array.mapWithIndex((index, char) => {
          switch Int.fromString(char) {
          | Some(_) => Some(index)
          | None => None
          }
        })
        ->Array.keepMap(v => v)
      let stacks = List.toArray(stacks)
      charIndexes->Array.map(charIndex => {
        stacks
        ->Array.map(stack => stack->Js.String2.charAt(charIndex))
        ->Array.keep(char => char->Js.String2.trim != "")
      })
    }
  }

let parseInput = input =>
  switch input->Js.String2.split("\n\n") {
  | [crates, moves] =>
    Some({
      crates: crates->parseCrates,
      moves: moves->Js.String2.split("\n")->Array.keepMap(parseMove),
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
      result->Array.set(to, toStack->Array.concat(moveApart->Array.reverse))->ignore
      result
    })
    ->Array.keepMap(crate => crate->Array.get(Array.length(crate) - 1))
    ->Js.Array2.joinWith("")
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
      result->Array.set(to, toStack->Array.concat(moveApart))->ignore
      result
    })
    ->Array.keepMap(crate => crate->Array.get(Array.length(crate) - 1))
    ->Js.Array2.joinWith("")
  })

Js.log({"part1": part1, "part2": part2})
