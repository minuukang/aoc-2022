let getPriority = char => {
  let code = char->Js.String2.charCodeAt(0)->Float.toInt
  if code >= 65 && code <= 90 {
    // A-Z
    Some(code - 38) // 27 through 52.
  } else if code >= 97 && code <= 122 {
    Some(code - 96) // 1 through 26.
  } else {
    None
  }
}

let parseInput = input =>
  input
  ->Js.String2.split("\n")
  ->Array.map(line => line->Js.String2.split("")->Array.keepMap(getPriority))

let part1 =
  Utils.readInput("Day3.part1.txt")
  ->parseInput
  ->Array.map(items => {
    let len = Array.length(items)
    (
      items->Array.slice(~offset=0, ~len=len / 2)->Set.Int.fromArray,
      items->Array.slice(~offset=len / 2, ~len)->Set.Int.fromArray,
    )
  })
  ->Array.map(((first, second)) => {
    Set.Int.intersect(first, second)->Set.Int.toArray
  })
  ->Array.reduce([], (acc, prioritys) => acc->Array.concat(prioritys))
  ->Utils.sumIntArray

let part2 =
  Utils.readInput("Day3.part2.txt")
  ->parseInput
  ->Utils.chunkArray(~step=3)
  ->Array.keepMap(group => {
    switch group->List.fromArray {
    | list{first, ...rest} =>
      Some(
        rest
        ->List.toArray
        ->Array.reduce(first->Set.Int.fromArray, (acc, items) =>
          Set.Int.intersect(acc, Set.Int.fromArray(items))
        )
        ->Set.Int.toArray,
      )
    | list{} => None
    }
  })
  ->Array.reduce([], (acc, prioritys) => acc->Array.concat(prioritys))
  ->Utils.sumIntArray

Js.log({
  "part1": part1,
  "part2": part2,
})
