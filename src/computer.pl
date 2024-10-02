:- module(computer, [
  computer_move/0
]).

:- use_module(game_board).
:- use_module(player).
:- use_module(move).
:- use_module(capture).
:- use_module(utils).


computer_move() :-
  board(Board),
  current_player(Player),
  Player == b,

  find_piece_positions(b, BluePiecePositions),
  find_piece_positions(bq, BlueQueenPiecePositions),
  % BlueQueenPiecePositions = [],
  append(BluePiecePositions, BlueQueenPiecePositions, FromPositions),
  
  findall(
    [move, Value, Coord1, Coord2],
    (
      member([FromRow, FromColumn], FromPositions),
      get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
      get_all_valid_moves(FromRow, FromColumn, FromPiece, DestinationPositions),
      member([ToRow, ToColumn], DestinationPositions),
      make_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, NewBoard),
      evaluate_board(NewBoard, Value),
      position_to_coord([FromRow, FromColumn], Coord1),
      position_to_coord([ToRow, ToColumn], Coord2)
    ),
    EvaluationsMoves
  ),

  write(EvaluationsMoves), nl,


  findall(
    [capture, Value, Coord1, Coords],
    (
      member([FromRow, FromColumn], FromPositions),
      longest_capture_sequence(FromRow, FromColumn, Sequence),

      position_to_coord([FromRow, FromColumn], Coord1),
      maplist(position_to_coord, Sequence, Coords),
      
      make_capture(Board, Coord1, Coords, NewBoard),
      evaluate_board(NewBoard, Value)
    ),
    EvaluationsCaptures
  ),

  append(EvaluationsMoves, EvaluationsCaptures, Evaluations),

  sort_matrix_by_column(Evaluations, 1, EvaluationsSorted),
  get_last_element(EvaluationsSorted, BestEvaluation),

  get_head(BestEvaluation, ActionType),  
  (ActionType == move
    -> (
      get_element_at_index(BestEvaluation, 2, MoveCoord1),
      get_element_at_index(BestEvaluation, 3, MoveCoord2),
      mv(MoveCoord1, MoveCoord2)
    ) ; (
      get_element_at_index(BestEvaluation, 2, CaptureCoord1),
      get_element_at_index(BestEvaluation, 3, CaptureCoords),
      cap(CaptureCoord1, CaptureCoords)
    )
  ).


evaluate_board(Board, Value) :- process_matrix(Board, piece_value, 0, Value).


process_row([], _, Counter, Counter).

process_row([Element | Tail], CounterFunction, Counter, FinalCounter) :-
  call(CounterFunction, Element, Increment),
  
  length(Tail, TailLength),
  (Element == b
    -> RankBonus is 8 - TailLength
    ; (Element == r) -> RankBonus is TailLength - 8 ; RankBonus is 0
  ),
  
  UpdatedCounter is Counter + Increment + RankBonus,
  process_row(Tail, CounterFunction, UpdatedCounter, FinalCounter).


process_matrix([], _, Counter, Counter).

process_matrix([Row | Tail], CounterFunction, Counter, FinalCounter) :-
  process_row(Row, CounterFunction, Counter, UpdatedCounter),
  process_matrix(Tail, CounterFunction, UpdatedCounter, FinalCounter).

