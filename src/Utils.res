let sumIntArray = arr => arr->Array.reduce(0, (acc, val) => acc + val)
let stringMatchAll = (str, regexp) => {
  let break = ref(false)
  let matches = []
  while !break.contents {
    switch regexp->Js.Re.exec_(str) {
    | None => break := true
    | Some(result) => matches->Array.push(result)
    }
  }
  matches
}
let chunkArray = (arr, ~step) =>
  Array.rangeBy(0, Array.length(arr) - 1, ~step)->Array.map(offset => {
    arr->Js.Array2.slice(~start=offset, ~end_=offset + step)
  })

let readInput = filename => {
  NodeJs.Fs.readFileSync(
    NodeJs.Global.dirname->NodeJs.Path.join2("./input/" ++ filename),
  )->Node.Buffer.toString
}
