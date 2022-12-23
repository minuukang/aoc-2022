let sumIntArray = arr => arr->Array.reduce(0, (acc, val) => acc + val)

let readInput = filename => {
  NodeJs.Fs.readFileSync(
    NodeJs.Global.dirname->NodeJs.Path.join2("./input/" ++ filename),
  )->Node.Buffer.toString
}
