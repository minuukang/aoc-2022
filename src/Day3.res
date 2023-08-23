let getPriority = char => {
  let code = char->String.charCodeAt(0)->Float.toInt
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
  input->String.split("\n")->Array.map(line => line->String.split("")->Array.filterMap(getPriority))

// execute
let textInput = await Utils.readInputAsync("Day3.txt")

// Set.fromArray([])

let part1 =
  textInput
  ->parseInput
  ->Array.map(items => {
    let len = Array.length(items)
    (
      items->Array.slice(~start=0, ~end=len / 2)->Set.fromArray,
      items->Array.slice(~start=len / 2, ~end=len)->Set.fromArray,
    )
  })
  ->Array.map(((first, second)) => {
    Utils.Set.intersect(first, second)->Set.values->Iterator.toArray
  })
  ->Array.reduce([], (acc, prioritys) => acc->Array.concat(prioritys))
  ->Utils.sumIntArray

let part2 =
  textInput
  ->parseInput
  ->Utils.chunkArray(~step=3)
  ->Array.filterMap(group => {
    switch group->List.fromArray {
    | list{first, ...rest} =>
      Some(
        rest
        ->List.toArray
        ->Array.reduce(first->Set.fromArray, (acc, items) =>
          Utils.Set.intersect(acc, items->Set.fromArray)
        )
        ->Set.values
        ->Iterator.toArray,
      )
    | list{} => None
    }
  })
  ->Array.reduce([], (acc, prioritys) => acc->Array.concat(prioritys))
  ->Utils.sumIntArray

Console.log({
  "part1": part1,
  "part2": part2,
})
