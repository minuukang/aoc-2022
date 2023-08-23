let parseBuffer = (stream, bufferSize) => {
  Belt.Array.range(0, stream->Array.length)
  ->Array.find(current => {
    let seq = Set.fromArray(
      Belt.Array.range(current, current + (bufferSize - 1))->Array.filterMap(index =>
        stream->Array.get(index)
      ),
    )
    seq->Set.size == bufferSize
  })
  ->Option.map(position => position + bufferSize)
}

let input = (await Utils.readFilePromise("day6.txt"))->String.split("")

// part1
input->parseBuffer(4)->Console.log

// part2
input->parseBuffer(14)->Console.log
