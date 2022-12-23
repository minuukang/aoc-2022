let readInput = filename => {
  NodeJs.Fs.readFileSync(
    NodeJs.Global.dirname->NodeJs.Path.join2("./input/" ++ filename),
  )->Node.Buffer.toString
}
