let sumIntArray = arr => arr->Array.reduce(0, (acc, val) => acc + val)

let stringMatchAll = (str, regexp) => {
  let break = ref(false)
  let matches = []
  while !break.contents {
    switch regexp->RegExp.exec(str) {
    | None => break := true
    | Some(result) => matches->Array.push(result)
    }
  }
  matches
}
let chunkArray = (arr, ~step) =>
  Belt.Array.rangeBy(0, Array.length(arr) - 1, ~step)->Belt.Array.map(offset => {
    arr->Js.Array2.slice(~start=offset, ~end_=offset + step)
  })

let readInput = filename => {
  NodeJs.Fs.readFileSync(
    NodeJs.Global.dirname->NodeJs.Path.join2("./input/" ++ filename),
  )->NodeJs.Buffer.toString
}

@module("fs/promise") external readFilePromise: string => promise<string> = "readFile"

let readInputAsync = async filename => {
  await readFilePromise(NodeJs.Global.dirname->NodeJs.Path.join2(`./input/${filename}`))
}

module Set = {
  let intersect = (a, b) => {
    Set.fromArray(a->Set.values->Iterator.toArray->Array.filter(v => b->Set.has(v)))
  }
}
