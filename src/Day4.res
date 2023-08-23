let parsePair = pair =>
  switch pair->String.split("-")->Array.filterMap(v => Int.fromString(v)) {
  | [min, max] => Some(Belt.Array.range(min, max)->Set.fromArray)
  | _ => None
  }

let parseInput = input =>
  input
  ->String.split("\n")
  ->Array.filterMap(line =>
    switch line->String.split(",")->Array.map(parsePair) {
    | [Some(sections1), Some(sections2)] => Some((sections1, sections2))
    | _ => None
    }
  )

let part1 =
  Utils.readInput("Day4.txt")
  ->parseInput
  ->Array.filter(((sections1, sections2)) =>
    sections1->Utils.Set.intersect(sections2)->Set.size ==
      Math.Int.min(sections1->Set.size, sections2->Set.size)
  )
  ->Array.length

let part2 =
  Utils.readInput("Day4.txt")
  ->parseInput
  ->Belt.Array.keep(((sections1, sections2)) =>
    sections1->Utils.Set.intersect(sections2)->Set.size > 0
  )
  ->Array.length

Console.log({
  "part1": part1,
  "part2": part2,
})
