let parseElfCalorie = input =>
  input
  ->String.split("\n\n")
  ->Array.map(elf => elf->String.split("\n")->Array.filterMap(line => Int.fromString(line)))
  ->Array.map(calories => calories->Utils.sumIntArray)

let fileInput = await Utils.readInputAsync("Day1.txt")

let part1 = fileInput->parseElfCalorie->Math.Int.maxMany
let part2 =
  fileInput
  ->parseElfCalorie
  ->Array.toSorted((a, b) => Float.fromInt(b - a))
  ->Array.slice(~start=0, ~end=3)
  ->Utils.sumIntArray

Console.log({
  "part1": part1,
  "part2": part2,
})
