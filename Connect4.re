open!  CS17SetupGame;   
open SigGame; 

module Connect4 = {

      type whichPlayer =
      | P1
      | P2;
type status =
  | Win(whichPlayer)
  | Draw
  | Ongoing(whichPlayer);
type squareState =
  | Red
  | Blue
  | Empty;
type state =
  | State(status, list(list(squareState)));
let initialRows = 5;
let initialCols = 7;
/* makeList 'a * int => list('a)
Input: an input 'a, and a positive int
Output: a list consisting of the input of the length of the input int
Recursion diagram:
OI: "a" * 3
 RI: "a" * 2
 RO: ["a", "a"]
 The input 'a is then consed onto the beginning
OO: ["a", "a", "a"]
*/
let rec makeList: ('a, int) => list('a) =
  (input, length) =>
    switch (length) {
    | 0 => []
    | _ => [input, ...makeList(input, length - 1)]
    };
let initialState =
  State(Ongoing(P1), makeList(makeList(Empty, initialRows), initialCols));

    /* TODO: implement your game with the rest of the Game signature */
    type move = int;
    /* stringOfState: state => string
    Input: a state
    Output: the string representing that state visually
    Recursion diagram:
    OI: State(Ongoing(P1), [[Empty, Empty], [Empty, Red], [Empty, Empty]])
      RI: State(Ongoing(P1), [[Empty], [Red], Empty])
      RO: |   | ■ |   |
      The original head is then appended on to the beginning with a newline
    OO: |   |   |   |\n|   | ■ |   |
    */
    let rec stringOfState: state => string =
  input => {
    let rec listToString: list(squareState) => string =
      input =>
        switch (input) {
        | [] => ""
        | [hd, ...tl] =>
        let squareString = {js|■|js};
          switch (hd) {
          | Red => "\027[31m" ++ squareString ++ "\027[0m | " ++ 
          listToString(tl)
          | Blue => "\027[34m"++ squareString ++ "\027[0m | " ++ 
          listToString(tl)
          | Empty => "  | " ++ listToString(tl)
          }
        };
    let empties = makeList([], initialCols);
    switch (input) {
    | State(inStatus, inBoard) =>
      if (inBoard == empties) {
        "";
      } else {
        "| "
        ++ listToString(List.map(List.hd, inBoard))
        ++ "\n"
        ++ stringOfState(State(inStatus, List.map(List.tl, inBoard)));
      }
    };
  };
  /* legalMoves: state => list(move)
  Input: a state
  Output: the legal moves for that state
  */
  let legalMoves: state => list(move) =
  inState => {
    let rec colCheck: (int, list(list(squareState))) => list(move) =
      (n, inList) =>
        switch (inList) {
        | [] => []
        | [hd, ...tl] =>
          if (List.hd(hd) == Empty) {
            [n, ...colCheck(n + 1, tl)];
          } else {
            colCheck(n + 1, tl);
          }
        };
    switch (inState) {
    | State(_, inBoard) => colCheck(1, inBoard)
    };
  };
  /* nextState: state * move => state
  Input: a state and a move
  Output: the state that results from applying that move to the previous state
  */
  let nextState: (state, move) => state =
  (inState, inMove) => {
    let rec tokenFall: (list(squareState), squareState) => list(squareState) =
      (inCol, tok) =>
        switch (inCol) {
        | [] => failwith("Bad column")
        | [hd] =>
          if (hd == Empty) {
            [tok];
          } else {
            failwith("Bad column");
          }
        | [hd, hd2, ...tl] =>
          if (hd2 != Empty) {
            [tok, hd2, ...tl];
          } else {
            [hd, ...tokenFall([hd2, ...tl], tok)];
          }
        };
    let rec newBoard:
      (list(list(squareState)), int, squareState) =>
      list(list(squareState)) =
      (inBoard, n, tokenColor) =>
        switch (inBoard) {
        | [] => []
        | [hd, ...tl] =>
          if (n == inMove) {
            [tokenFall(hd, tokenColor), ...newBoard(tl, n + 1, tokenColor)];
          } else {
            [hd, ...newBoard(tl, n + 1, tokenColor)];
          }
        };
    let rec getRowNum: (list(squareState), int) => int =
      (inCol, n) =>
        switch (inCol) {
        | [] => n
        | [hd, ...tl] =>
          if (hd != Empty) {
            n;
          } else {
            getRowNum(tl, n + 1);
          }
        };
    let rec zip2: (('a, 'b) => 'b, list('a), list('b)) => list('b) =
      (f, items, things) =>
        switch (items, things) {
        | ([], _) => things
        | ([hd1, ...tl1], [hd2, ...tl2]) => [
            f(hd1, hd2),
            ...zip2(f, tl1, tl2),
          ]
        | (_, []) => failwith("Domain error")
        };
    let rec upDiags = input =>
      switch (input) {
      | [] => failwith("Domain error: empty matrix")
      | [row] => List.map(x => [x], row)
      | [[], ...otherRows] => failwith("Domain error")
      | [[hd, ...tl], ...otherRows] => [
          [hd],
          ...zip2((x, y) => [x, ...y], tl, upDiags(otherRows)),
        ]
      };
    let checkList: list(squareState) => bool =
      inRow => {
        let rec checkListHelper: (list(squareState), int) => bool =
          (row, current) =>
            switch (row, current) {
            | (_, 3) => true
            | ([], _) => false
            | ([_], _) => false
            | ([hd, hd1, ...tl], n) =>
              if (hd == hd1 && hd == Red || hd == hd1 && hd == Blue) {
                checkListHelper([hd1, ...tl], n + 1);
              } else {
                checkListHelper([hd1, ...tl], 0);
              }
            };
        checkListHelper(inRow, 0);
      };
    let checkUpDiag: list(list(squareState)) => bool =
      board => {
        let upDiagList = upDiags(board);
        List.mem(true, List.map(checkList, upDiagList));
      };
    let checkDownDiag: list(list(squareState)) => bool =
      board => {
        let flippedBoard = List.map(List.rev, board);
        let downDiagList = upDiags(flippedBoard);
        List.mem(true, List.map(checkList, downDiagList));
      };
    let checkCol: list(list(squareState)) => bool =
      input => {
        let col = List.nth(input, inMove - 1);
        checkList(col);
      };
    let checkRow: list(list(squareState)) => bool =
      input => {
        let rowNum = getRowNum(List.nth(input, inMove - 1), 0);
        let rowHelper: list('a) => 'a = input1 => List.nth(input1, rowNum);
        let row = List.map(rowHelper, input);
        checkList(row);
      };
    let checkStatus: list(list(squareState)) => bool =
      input =>
        checkCol(input)
        || checkRow(input)
        || checkUpDiag(input)
        || checkDownDiag(input);
    switch (inState) {
    | State(Win(_), _) => inState;
    | State(Draw, _) => inState;
    | State(Ongoing(player), board) =>
      let rec checkDraw: list(list(squareState)) => bool = inBoard => 
      switch(inBoard) {
        | [] => true
        | [hd, ...tl] => 
        if (List.hd(hd)==Empty) {false} else {checkDraw(tl)}
      };
      if (player == P1) {
        let recentBoard = newBoard(board, 1, Red);
        if (checkDraw(recentBoard)) {
          State(Draw, recentBoard)
        } else 
        if (checkStatus(recentBoard)) {
          State(Win(P1), recentBoard);
        } else {
          State(Ongoing(P2), recentBoard);
        };
      } else {
        let recentBoard = newBoard(board, 1, Blue);
        if (checkDraw(recentBoard)) {
          State(Draw, recentBoard)
        } else 
        if (checkStatus(recentBoard)) {
          State(Win(P2), recentBoard);
        } else {
          State(Ongoing(P1), recentBoard);
        };
      }
    };
  };
  /* moveOfString: string => move
  Input: a string
  Output: the move represented by that string
  */
  let moveOfString: string => move = input => int_of_string(input);
  /* gameStatus: state => status
  Input: a state
  Output: the status contained by that state
  */
  let gameStatus: state => status = input => switch(input) {
    | State(outStatus, _) => outStatus;
  }
  /* stringOfMove: move => string
  Input: a move
  Output: the string represented by that move
  */
  let stringOfMove: move => string = input => string_of_int(input);
  /* stringOfPlayer: whichPlayer => string
  Input: a whichPlayer
  Output: the player represented by that whichPlayer
  */
  let stringOfPlayer: whichPlayer => string = input => switch(input) {
    | P1 => "Player 1"
    | P2 => "Player 2"
  }
  /* estimateValue: state => float
  Input: a state
  Output: The estimated relative value of that state, assuming P1 as the 
  maximizing player
  */
  let estimateValue: state => float =
  inState => {
    let rec colCounter: (whichPlayer, list(squareState)) => int =
      (inPlayer, input) =>
        switch (input) {
        | [] => 0
        | [hd, ...tl] =>
          switch (hd) {
          | Empty => 1 + colCounter(inPlayer, tl)
          | _ =>
            switch (inPlayer) {
            | P1 =>
              if (hd == Red) {
                1 + colCounter(inPlayer, tl);
              } else {
                0;
              }
            | P2 =>
              if (hd == Blue) {
                1 + colCounter(inPlayer, tl);
              } else {
                0;
              }
            }
          }
        };
    let getValue: int => float =
      input =>
        switch (input) {
        | 4 => 1000000.0
        | 3 => 9.0
        | 2 => 4.0
        | 1 => 1.0
        | 0 => 0.0
        | _ => failwith("already won")
        };
    let estimateCol: (whichPlayer, list(squareState)) => float =
      (inPlayer, inCol) => {
        let rec colHelper: (int, list(squareState)) => float =
          (currentLen, input) =>
            switch (currentLen, input) {
            | (n, []) => failwith("Empty column")
            | (n, [hd]) =>
              switch (inPlayer) {
              | P1 =>
                switch (hd) {
                | Empty => 0.0
                | Red => getValue(n + 1)
                | Blue => getValue(n)
                }
              | P2 =>
                switch (hd) {
                | Empty => 0.0
                | Red => getValue(n)
                | Blue => getValue(n + 1)
                }
              }
            | (n, [hd, hd1, ...tl]) =>
              switch (inPlayer) {
              | P1 =>
                switch (hd) {
                | Empty => colHelper(0, [hd1, ...tl])
                | Red =>
                  if (hd == hd1) {
                    colHelper(n + 1, [hd1, ...tl]);
                  } else {
                    getValue(n + 1);
                  }
                | Blue => 0.0
                }
              | P2 =>
                switch (hd) {
                | Empty => colHelper(0, [hd1, ...tl])
                | Blue =>
                  if (hd == hd1) {
                    colHelper(n + 1, [hd1, ...tl]);
                  } else {
                    getValue(n + 1);
                  }
                | Red => 0.0
                }
              }
            };
        if (colCounter(inPlayer, inCol) >= 4) {
          colHelper(0, inCol);
        } else {
          0.0;
        };
      };
    let estimateRow: (whichPlayer, list(squareState)) => float =
      (inPlayer, inRow) => {
        let rec rowHelper: (int, list(squareState)) => float =
          (currentLen, input) =>
            switch (currentLen, input) {
            | (n, []) => failwith("Empty column")
            | (n, [hd]) =>
              switch (inPlayer) {
              | P1 =>
                switch (hd) {
                | Empty => 0.0
                | Red => getValue(n + 1)
                | Blue => getValue(n)
                }
              | P2 =>
                switch (hd) {
                | Empty => 0.0
                | Red => getValue(n)
                | Blue => getValue(n + 1)
                }
              }
            | (n, [hd, hd1, ...tl]) =>
              switch (inPlayer) {
              | P1 =>
                switch (hd) {
                | Empty => rowHelper(0, [hd1, ...tl])
                | Red =>
                  if (hd == hd1) {
                    rowHelper(n + 1, [hd1, ...tl]);
                  } else {
                    getValue(n + 1) +. rowHelper(0, [hd1, ...tl]);
                  }
                | Blue => rowHelper(0, [hd1, ...tl])
                }
              | P2 =>
                switch (hd) {
                | Empty => rowHelper(0, [hd1, ...tl])
                | Blue =>
                  if (hd == hd1) {
                    rowHelper(n + 1, [hd1, ...tl]);
                  } else {
                    getValue(n + 1) +. rowHelper(0, [hd1, ...tl]);
                  }
                | Red => rowHelper(0, [hd1, ...tl])
                }
              }
            };
        rowHelper(0, inRow);
      };
    let rec transpose: list(list(squareState)) => list(list(squareState)) =
      matrix =>
        switch (matrix) {
        | []
        | [[], ..._] => failwith("A matrix cannot be 0-dimensional.")
        | [[_], ..._] => [List.flatten(matrix)]
        | [[_, ..._], ..._] => [
            List.map(List.hd, matrix),
            ...transpose(List.map(List.tl, matrix)),
          ]
        };
    let func: (float, float) => float = (in1, in2) => in1 +. in2;
    let rec zip2: (('a, 'b) => 'b, list('a), list('b)) => list('b) =
      (f, items, things) =>
        switch (items, things) {
        | ([], _) => things
        | ([hd1, ...tl1], [hd2, ...tl2]) => [
            f(hd1, hd2),
            ...zip2(f, tl1, tl2),
          ]
        | (_, []) => failwith("Domain error")
        };
    let rec upDiags = input =>
      switch (input) {
      | [] => failwith("Domain error: empty matrix")
      | [row] => List.map(x => [x], row)
      | [[], ...otherRows] => failwith("Domain error")
      | [[hd, ...tl], ...otherRows] => [
          [hd],
          ...zip2((x, y) => [x, ...y], tl, upDiags(otherRows)),
        ]
      };
    switch (inState) {
    | State(Win(P1), _) => 1000000000.0
    | State(Win(P2), _) => -1000000000.0
    | State(Draw, _) => 0.0
    | State(Ongoing(inPlayer), board) => switch(inPlayer) {
      | P1 => 1.0*.(List.fold_right(func, 
      List.map(estimateCol(inPlayer), board), 0.0)
      +. List.fold_right(
           func,
           List.map(estimateRow(inPlayer), transpose(board)),
           0.0,
         )
      +. List.fold_right(
           func,
           List.map(estimateRow(inPlayer), upDiags(board)),
           0.0,
         )
      +. List.fold_right(
           func,
           List.map(
             estimateRow(inPlayer),
             upDiags(List.map(List.rev, board)),
           ),
           0.0,
         ))
      | P2 => -1.0*.(List.fold_right(func, 
      List.map(estimateCol(inPlayer), board), 0.0)
      +. List.fold_right(
           func,
           List.map(estimateRow(inPlayer), transpose(board)),
           0.0,
         )
      +. List.fold_right(
           func,
           List.map(estimateRow(inPlayer), upDiags(board)),
           0.0,
         )
      +. List.fold_right(
           func,
           List.map(
             estimateRow(inPlayer),
             upDiags(List.map(List.rev, board)),
           ),
           0.0,
         ))
    }
    };
  };

};

module MyGame : Game = Connect4;
open Connect4;

/* test cases */
checkExpect(stringOfPlayer(P1), "Player 1", "test player 1");
checkExpect(stringOfPlayer(P2), "Player 2", "test player 2");
 checkExpect(stringOfMove(3), "3", "generic test stringOfMove");
  checkExpect(stringOfMove(0), "0", "0 test stringOfMove");
  checkExpect(stringOfMove(2), "2", "generic test 2 stringOfMove");
checkExpect(makeList(3, 3), [3, 3, 3], "generic test with ints");
checkExpect(makeList("a", 0), [], "test with length 0");
checkExpect(makeList(Red, 4), [Red, Red, Red, Red], 
"generic test with squareState");
  checkExpect(moveOfString("3"), 3, "generic move test");
  checkExpect(moveOfString("0"), 0, "test with 0");
  checkExpect(moveOfString("7"), 7, "second generic test");
