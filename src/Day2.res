type shape = Rock | Scissors | Paper
type status = Win | Lose | Draw

let shapeScore = shape =>
  switch shape {
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  }

let playScore = status =>
  switch status {
  | Win => 6
  | Draw => 3
  | Lose => 0
  }

let parseElfShape = char =>
  switch char {
  | "A" => Some(Rock)
  | "B" => Some(Paper)
  | "C" => Some(Scissors)
  | _ => None
  }

let parsePlayerShape = char =>
  switch char {
  | "X" => Some(Rock)
  | "Y" => Some(Paper)
  | "Z" => Some(Scissors)
  | _ => None
  }

let play = (playerShape, elfShape) => {
  switch (playerShape, elfShape) {
  | (Rock, Scissors)
  | (Scissors, Paper)
  | (Paper, Rock) =>
    Win
  | (Scissors, Rock)
  | (Paper, Scissors)
  | (Rock, Paper) =>
    Lose
  | (Rock, Rock)
  | (Scissors, Scissors)
  | (Paper, Paper) =>
    Draw
  }
}

let parsePlayerNecessaryStatus = char =>
  switch char {
  | "X" => Some(Lose)
  | "Y" => Some(Draw)
  | "Z" => Some(Win)
  | _ => None
  }

let getNecessaryShape = (status, elfShape) => {
  switch (status, elfShape) {
  | (Win, Rock)
  | (Draw, Paper)
  | (Lose, Scissors) =>
    Paper
  | (Win, Scissors)
  | (Draw, Rock)
  | (Lose, Paper) =>
    Rock
  | (Win, Paper)
  | (Draw, Scissors)
  | (Lose, Rock) =>
    Scissors
  }
}

type game = {
  elfShape: shape,
  playerShape: shape,
}

let resultGameScore = game => {
  play(game.playerShape, game.elfShape)->playScore + shapeScore(game.playerShape)
}

type inferenceGame = {
  elfShape: shape,
  playerStatus: status,
}

let part1 =
  Utils.readInput("Day2.part1.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(line =>
    switch line->Js.String2.split(" ") {
    | [char1, char2] =>
      switch (parseElfShape(char1), parsePlayerShape(char2)) {
      | (Some(elfShape), Some(playerShape)) =>
        Some({
          elfShape,
          playerShape,
        })
      | _ => None
      }
    | _ => None
    }
  )
  ->Array.map(resultGameScore)
  ->Utils.sumIntArray

let part2 =
  Utils.readInput("Day2.part2.txt")
  ->Js.String2.split("\n")
  ->Array.keepMap(line =>
    switch line->Js.String2.split(" ") {
    | [char1, char2] =>
      switch (parseElfShape(char1), parsePlayerNecessaryStatus(char2)) {
      | (Some(elfShape), Some(playerStatus)) =>
        Some({
          elfShape,
          playerStatus,
        })
      | _ => None
      }
    | _ => None
    }
  )
  ->Array.map(({elfShape, playerStatus}) =>
    resultGameScore({
      playerShape: getNecessaryShape(playerStatus, elfShape),
      elfShape,
    })
  )
  ->Utils.sumIntArray

Js.log({
  "part1": part1,
  "part2": part2,
})
