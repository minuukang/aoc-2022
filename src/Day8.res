type item = {
  x: int,
  y: int,
  value: int,
}

type grid = {
  xLength: int,
  yLength: int,
  items: array<item>,
}

let makeGrid = items => {
  xLength: items->Array.map(item => item.x)->Js.Math.maxMany_int,
  yLength: items->Array.map(item => item.y)->Js.Math.maxMany_int,
  items,
}

let isOutsideItem = (item, ~xLength, ~yLength) => {
  item.x == 0 || item.y == 0 || item.x == xLength || item.y == yLength
}

let getDirectionItems = (targetItem, ~grid) => {
  [
    // top
    grid.items
    ->Array.keep(item => targetItem.x == item.x && targetItem.y > item.y)
    ->Js.Array2.sortInPlaceWith((a, b) => b.y - a.y),
    // bottom
    grid.items
    ->Array.keep(item => targetItem.x == item.x && targetItem.y < item.y)
    ->Js.Array2.sortInPlaceWith((a, b) => a.y - b.y),
    // left
    grid.items
    ->Array.keep(item => targetItem.y == item.y && targetItem.x > item.x)
    ->Js.Array2.sortInPlaceWith((a, b) => b.x - a.x),
    // right
    grid.items
    ->Array.keep(item => targetItem.y == item.y && targetItem.x < item.x)
    ->Js.Array2.sortInPlaceWith((a, b) => a.x - b.x),
  ]
}

let isDirectionVisible = (targetItem, directionItems) => {
  directionItems->Array.map(item => item.value)->Js.Math.maxMany_int < targetItem.value
}

let getVisibleItems = grid => {
  grid.items->Array.keep(item => {
    isOutsideItem(item, ~xLength=grid.xLength, ~yLength=grid.yLength) ||
    getDirectionItems(item, ~grid)->Array.some(directionItems =>
      isDirectionVisible(item, directionItems)
    )
  })
}

let getItemScore = (targetItem, ~grid) => {
  getDirectionItems(targetItem, ~grid)
  ->Array.map(directionItems => {
    1 +
    directionItems
    ->Array.getIndexBy(item => {
      targetItem.value <= item.value
    })
    ->Option.getWithDefault(directionItems->Array.length - 1)
  })
  ->Array.reduce(1, (acc, score) => acc * score)
}

let getMaxScore = grid => {
  grid.items->Array.map(item => getItemScore(item, ~grid))->Js.Math.maxMany_int
}

let inputGrid =
  Utils.readInput("Day8.txt")
  ->Js.String2.split("\n")
  ->Array.mapWithIndex((y, line) =>
    line
    ->Js.String2.split("")
    ->Array.keepMap(Int.fromString)
    ->Array.mapWithIndex((x, value) => {
      x,
      y,
      value,
    })
  )
  ->Array.reduce([], (acc, items) => acc->Array.concat(items))
  ->makeGrid

// part1
inputGrid->getVisibleItems->Array.length->Js.log

// part2
inputGrid->getMaxScore->Js.log
