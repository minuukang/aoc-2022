let parseBuffer = (stream, bufferSize) => {
  Array.range(0, stream->Array.length)
  ->Array.getBy(current => {
    let seq = Set.String.fromArray(
      Array.range(current, current + (bufferSize - 1))->Array.keepMap(index =>
        stream->Array.get(index)
      ),
    )
    seq->Set.String.size == bufferSize
  })
  ->Option.map(position => position + bufferSize)
}

let input = Utils.readInput("day6.txt")->Js.String2.split("")

// part1
input->parseBuffer(4)->Js.log

// part2
input->parseBuffer(14)->Js.log
