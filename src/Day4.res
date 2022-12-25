let parsePair = pair =>
  switch pair->Js.String2.split("-")->Array.keepMap(Int.fromString) {
  | [min, max] => Some(Array.range(min, max)->Set.Int.fromArray)
  | _ => None
  }

let parseInput = input =>
  input
  ->Js.String2.split("\n")
  ->Array.keepMap(line =>
    switch line->Js.String2.split(",")->Array.map(parsePair) {
    | [Some(sections1), Some(sections2)] => Some((sections1, sections2))
    | _ => None
    }
  )

let part1 =
  Utils.readInput("Day4.txt")
  ->parseInput
  ->Array.keep(((sections1, sections2)) =>
    sections1->Set.Int.intersect(sections2)->Set.Int.size ==
      Js.Math.min_int(sections1->Set.Int.size, sections2->Set.Int.size)
  )
  ->Array.length

let part2 =
  Utils.readInput("Day4.txt")
  ->parseInput
  ->Array.keep(((sections1, sections2)) =>
    sections1->Set.Int.intersect(sections2)->Set.Int.size > 0
  )
  ->Array.length

Js.log({
  "part1": part1,
  "part2": part2,
})
