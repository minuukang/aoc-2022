let sumIntArray = arr => arr->Array.reduce(0, (acc, val) => acc + val)

let parseElfCalorie = input =>
  input
  ->Js.String2.split("\n\n")
  ->Array.map(elf => elf->Js.String2.split("\n")->Array.keepMap(Int.fromString))
  ->Array.map(calories => calories->sumIntArray)

let part1 = Utils.readInput("Day1.part1.txt")->parseElfCalorie->Js.Math.maxMany_int
let part2 =
  Utils.readInput("Day1.part2.txt")
  ->parseElfCalorie
  ->Js.Array2.sortInPlaceWith((a, b) => b - a)
  ->Array.slice(~offset=0, ~len=3)
  ->sumIntArray

Js.log({
  "part1": part1,
  "part2": part2,
})
