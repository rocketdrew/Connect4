open! CS17SetupGame;
open SigGame; 
open Connect4;
module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame
    /* nextMove: state => move
    Input: a state
    Output: the optimal move for the current player to make from that state
    */
    let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      let depth = 5;
      let multiplier = ref(1.0);
      switch(PlayerGame.gameStatus(s)) {
        | Ongoing(P2) => multiplier := -1.0;
        | _ => multiplier := 1.0;
      };
      let rec tupleMax: list((float, float, float)) => (float, float, float) =
        inList =>
          switch (inList) {
          | [] => failwith("Empty list to max")
          | [hd] => hd
          | [hd, ...tl] => max(hd, tupleMax(tl))
          };
      let rec tupleMin: list((float, float, float)) => (float, float, float) =
        inList =>
          switch (inList) {
          | [] => failwith("Empty list to min")
          | [hd] => hd
          | [hd, ...tl] => min(hd, tupleMin(tl))
          };
      let rec minimax: (PlayerGame.state, int, PlayerGame.move, 
      ref(float), ref(float)) => (float, float, float) =
        (minState, curDepth, curMove, alpha1, beta1) => {
          let maxTree: (PlayerGame.state, int, ref(float), ref(float), 
          PlayerGame.move) => (float, float, float) =
            (maxState, currentDepth, alpha, beta, currentMove) => {
              if (currentDepth == depth || 
              PlayerGame.legalMoves(maxState) == []) {
                let currentValue = 
                PlayerGame.estimateValue(
                  PlayerGame.nextState(maxState, currentMove));
                let currentAlpha = max(alpha^, currentValue);
                (
                  currentValue *. multiplier^, currentAlpha, beta^
                );
              } else {
                let rec constructor: list(PlayerGame.move) => 
                list((float, float, float)) = inList =>
                switch(inList) {
                  | [] => []
                  | [hd, ...tl] => 
                  let outValue = 
                  minimax(PlayerGame.nextState(maxState, currentMove), 
                  currentDepth+1, hd, alpha, beta);
                  [outValue, ...constructor(tl)]
                };
                tupleMax(constructor(PlayerGame.legalMoves(maxState)));
              };}
          if (curDepth == depth || PlayerGame.legalMoves(minState) == []) {
            let currentValue = PlayerGame.estimateValue(
              PlayerGame.nextState(minState, curMove));
            let currentBeta = min(beta1^, currentValue);
            (
              currentValue *. multiplier^, alpha1^, currentBeta
            );
          } else {
            let rec constructor1: list(PlayerGame.move) => 
            list((float, float, float)) = inList =>
                switch(inList) {
                  | [] => []
                  | [hd, ...tl] => 
                  let outValue = maxTree(
                    PlayerGame.nextState(minState, curMove), 
                    curDepth+1, alpha1, beta1, hd);
                  [outValue, ...constructor1(tl)]
                };
            tupleMin(
              constructor1(PlayerGame.legalMoves(minState))
            )
          };
        };
      let nextMoveHelper: PlayerGame.move => (float, PlayerGame.move) =
        inMove =>
          switch (minimax(s, 0, inMove, ref(neg_infinity), ref(infinity))) {
          | (outValue, _, _) => (outValue, inMove)
          };
      let rec tupleComp: list((float, PlayerGame.move)) => 
      (float, PlayerGame.move) =
        inList =>
          switch (inList) {
          | [] => failwith("Empty list to compare")
          | [hd] => hd
          | [hd, ...tl] => max(hd, tupleComp(tl))
          };
      let tupleList = List.map(nextMoveHelper, PlayerGame.legalMoves(s));
      switch (tupleComp(tupleList)) {
      | (_, outMove) => outMove
      };
    };
  
}

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame); 
open TestAIPlayer; 

/* insert test cases for any procedures that don't take in 
 * or return a state here */

