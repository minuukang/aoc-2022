@deriving(jsConverter)
type commands = [#cd | #ls]
@deriving(jsConverter)
type specDirectory = [#".." | #"/"]

type rec file = Directory(tree) | File(string, int)
and tree = {
  name: string,
  parent?: tree,
  children: array<file>,
}

let rec getDirectorySize = tree => {
  tree.children->Array.reduce(0, (totalSize, file) => {
    totalSize +
    switch file {
    | Directory(tree) => getDirectorySize(tree)
    | File(_, fileSize) => fileSize
    }
  })
}

let parseLsLine = (line, parent) =>
  switch line->Js.String2.split(" ") {
  | ["dir", name] =>
    Some(
      Directory({
        name,
        parent,
        children: [],
      }),
    )
  | [size, name] =>
    switch size->Int.fromString {
    | Some(size') => Some(File(name, size'))
    | None => None
    }
  | _ => None
  }

exception ParseError(Js.Re.result)

let parseInput = input => {
  let root = {
    name: "/",
    children: [],
  }
  input
  ->Utils.stringMatchAll(%re("/(\$ (cd) ([^\s]+)|\$ (ls)\n([^$]+))/g"))
  ->Array.reduce(Some(root), (current, command) => {
    let current = current->Option.getExn
    switch command->Js.Re.captures->Array.keepMap(Js.Nullable.toOption) {
    | [_, _, "cd", "/"] => Some(root)
    | [_, _, "cd", ".."] => current.parent
    | [_, _, "cd", name] =>
      current.children
      ->Array.keepMap(file =>
        switch file {
        | Directory(tree) => Some(tree)
        | File(_) => None
        }
      )
      ->Array.getBy(directoryTree => directoryTree.name == name)
    | [_, _, "ls", children] =>
      current.children
      ->Js.Array2.pushMany(
        children
        ->Js.String2.split("\n")
        ->Array.keepMap(line => {
          parseLsLine(line, current)
        }),
      )
      ->ignore
      Some(current)
    | _ => raise(ParseError(command))
    }
  })
  ->ignore
  root
}

let rec makeChildrenDirectorySizeMap = (map, tree, prefix) => {
  tree.children->Array.reduce(map, (map', file) => {
    switch file {
    | Directory(tree) =>
      map'
      ->Map.String.set(prefix ++ tree.name, getDirectorySize(tree))
      ->Map.String.mergeMany(
        makeChildrenDirectorySizeMap(map', tree, prefix ++ tree.name ++ "/")->Map.String.toArray,
      )

    | File(_) => map'
    }
  })
}

Utils.readInput("Day7.txt")
->parseInput
->Some
->Option.map(root => {
  let rootSize = getDirectorySize(root)
  let directorySizeMap = makeChildrenDirectorySizeMap(Map.String.empty, root, root.name)
  {
    "part1": directorySizeMap
    ->Map.String.valuesToArray
    ->Array.keep(size => size < 100000)
    ->Utils.sumIntArray,
    "part2": {
      let totalDiskSize = 70000000
      let needDiskSize = 30000000
      let needDeleteDiskSize = needDiskSize - (totalDiskSize - rootSize)
      directorySizeMap
      ->Map.String.valuesToArray
      ->Array.keep(size => size > needDeleteDiskSize)
      ->Js.Math.minMany_int
    },
  }
})
->Js.log
