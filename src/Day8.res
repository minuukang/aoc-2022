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
  xLength: items->Array.map(item => item.x)->Math.Int.maxMany,
  yLength: items->Array.map(item => item.y)->Math.Int.maxMany,
  items,
}

let isOutsideItem = (item, ~xLength, ~yLength) => {
  item.x == 0 || item.y == 0 || item.x == xLength || item.y == yLength
}

let getDirectionItems = (targetItem, ~grid) => {
  [
    // top
    grid.items
    ->Array.filter(item => targetItem.x == item.x && targetItem.y > item.y)
    ->Array.toSorted((a, b) => Int.compare(b.y, a.y)),
    // bottom
    grid.items
    ->Array.filter(item => targetItem.x == item.x && targetItem.y < item.y)
    ->Array.toSorted((a, b) => Int.compare(a.y, b.y)),
    // left
    grid.items
    ->Array.filter(item => targetItem.y == item.y && targetItem.x > item.x)
    ->Array.toSorted((a, b) => Int.compare(b.x, a.x)),
    // right
    grid.items
    ->Array.filter(item => targetItem.y == item.y && targetItem.x < item.x)
    ->Array.toSorted((a, b) => Int.compare(a.x, b.x)),
  ]
}

let isDirectionVisible = (targetItem, directionItems) => {
  directionItems->Array.map(item => item.value)->Math.Int.maxMany < targetItem.value
}

let getVisibleItems = grid => {
  grid.items->Array.filter(item => {
    isOutsideItem(item, ~xLength=grid.xLength, ~yLength=grid.yLength) ||
    getDirectionItems(item, ~grid)->Array.some(directionItems =>
      isDirectionVisible(item, directionItems)
    )
  })
}

let getItemScore = (targetItem, ~grid) => {
  getDirectionItems(targetItem, ~grid)
  ->Array.map(directionItems => {
    let index = directionItems->Array.findIndex(item => {
      targetItem.value <= item.value
    })
    1 + (index == -1 ? directionItems->Array.length - 1 : index)
  })
  ->Array.reduce(1, (acc, score) => acc * score)
}

let getMaxScore = grid => {
  grid.items->Array.map(item => getItemScore(item, ~grid))->Math.Int.maxMany
}

let inputGrid =
  (await Utils.readInputAsync("Day8.txt"))
  ->String.split("\n")
  ->Array.mapWithIndex((line, y) =>
    line
    ->String.split("")
    ->Array.filterMap(v => v->Belt.Int.fromString)
    ->Array.mapWithIndex((value, x) => {
      x,
      y,
      value,
    })
  )
  ->Array.reduce([], (acc, items) => acc->Array.concat(items))
  ->makeGrid

// part1
inputGrid->getVisibleItems->Array.length->Console.log

// part2
inputGrid->getMaxScore->Console.log
