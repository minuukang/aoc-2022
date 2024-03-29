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
  switch line->String.split(" ") {
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

exception ParseError(RegExp.Result.t)

let parseInput = input => {
  let root = {
    name: "/",
    children: [],
  }
  input
  ->Utils.stringMatchAll(%re("/(\$ (cd) ([^\s]+)|\$ (ls)\n([^$]+))/g"))
  ->Array.reduce(Some(root), (current, command) => {
    let current = current->Option.getExn
    switch command {
    | [_, _, "cd", "/"] => Some(root)
    | [_, _, "cd", ".."] => current.parent
    | [_, _, "cd", name] =>
      current.children
      ->Array.filterMap(file =>
        switch file {
        | Directory(tree) => Some(tree)
        | File(_) => None
        }
      )
      ->Array.find(directoryTree => directoryTree.name == name)
    | [_, _, "ls", children] =>
      current.children
      ->Array.pushMany(
        children->String.split("\n")->Array.filterMap(line => parseLsLine(line, current)),
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
    | Directory(tree) => {
        map'->Map.set(`${prefix}${tree.name}`, getDirectorySize(tree))
        Map.fromArray(
          Array.concat(
            map'->Map.entries->Iterator.toArray,
            makeChildrenDirectorySizeMap(map', tree, prefix ++ tree.name ++ "/")
            ->Map.entries
            ->Iterator.toArray,
          ),
        )
      }
    | File(_) => map'
    }
  })
}

let input = await Utils.readInputAsync("Day7.txt")

input
->parseInput
->Some
->Option.map(root => {
  let rootSize = getDirectorySize(root)
  let directorySizeMap = makeChildrenDirectorySizeMap(Map.make(), root, root.name)
  {
    "part1": directorySizeMap
    ->Map.values
    ->Iterator.toArray
    ->Array.filter(size => size < 100000)
    ->Utils.sumIntArray,
    "part2": {
      let totalDiskSize = 70000000
      let needDiskSize = 30000000
      let needDeleteDiskSize = needDiskSize - (totalDiskSize - rootSize)
      directorySizeMap
      ->Map.values
      ->Iterator.toArray
      ->Array.filter(size => size > needDeleteDiskSize)
      ->Math.Int.minMany
    },
  }
})
->Console.log
