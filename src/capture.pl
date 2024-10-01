:- module(capture, [
  cap/2
]).

:- use_module(game_board).
:- use_module(player).
:- use_module(move).


cap(_, []).
cap(COORD1, [COORD2 | COORDS]) :-
  board(Board),
  % longest_capture_sequence(COORD1, LongestSequence),
  % length(LongestSequence, LengthLongestSequence),
  % length([COORD2 | COORDS], LengthCapture),
  % LengthCapture =:= LengthLongestSequence,
  make_capture(Board, COORD1, [COORD2 | COORDS], NewBoard),
  retractall(board(_)),
  assertz(board(NewBoard)),
  update_state().


make_capture(Board, _, [], Board).
make_capture(Board, COORD1, [COORD2 | COORDS], NewBoard) :-
  coord_to_position(COORD1, FromRow, FromColumn),
  coord_to_position(COORD2, ToRow, ToColumn),
  make_single_capture(Board, [FromRow, FromColumn], [ToRow, ToColumn], COORDS, NewBoardUpdated),
  make_capture(NewBoardUpdated, COORD2, COORDS, NewBoard).


make_single_capture(Board, [FromRow, FromColumn], [ToRow, ToColumn], COORDS, NewBoard) :-
  get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
  is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, FromPiece, CaptureRow, CaptureColumn),
  replace_position(Board, FromRow, FromColumn, e, NewBoardAux),
  replace_position(NewBoardAux, CaptureRow, CaptureColumn, e, NewBoardAux2),
  ((is_empty_list(COORDS), is_promotion(FromPiece, ToRow)) -> (
      promotion_piece(FromPiece, PromotionPiece),
      replace_position(NewBoardAux2, ToRow, ToColumn, PromotionPiece, NewBoard)
    )
    ; replace_position(NewBoardAux2, ToRow, ToColumn, FromPiece, NewBoard)
  ).


is_valid_capture(Board, FromRow, FromColumn, [DeltaToRow, DeltaToColumn], [DeltaCaptureRow, DeltaCaptureColumn], FromPiece, [ToRow, ToColumn]) :-
  ToRow is FromRow + DeltaToRow,
  ToColumn is FromColumn + DeltaToColumn,
  CaptureRow is FromRow + DeltaCaptureRow,
  CaptureColumn is FromColumn + DeltaCaptureColumn,
  is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, FromPiece, CaptureRow, CaptureColumn).  


is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, r, CaptureRow, CaptureColumn) :-
  is_valid_capture_normal_piece(Board, FromRow, ToRow, FromColumn, ToColumn, r, CaptureRow, CaptureColumn).

is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, b, CaptureRow, CaptureColumn) :-
  is_valid_capture_normal_piece(Board, FromRow, ToRow, FromColumn, ToColumn, b, CaptureRow, CaptureColumn).

is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, bq, CaptureRow, CaptureColumn) :-
  is_valid_capture_queen_piece(Board, FromRow, ToRow, FromColumn, ToColumn, bq, CaptureRow, CaptureColumn).

is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, rq, CaptureRow, CaptureColumn) :-
  is_valid_capture_queen_piece(Board, FromRow, ToRow, FromColumn, ToColumn, rq, CaptureRow, CaptureColumn).
  

is_valid_capture_normal_piece(Board, FromRow, ToRow, FromColumn, ToColumn, FromPiece, CaptureRow, CaptureColumn) :- 
  FromRow =\= ToRow,
  FromColumn =\= ToColumn,
  is_inside_boundaries(ToRow, ToColumn),
  is_valid_capture_diagonals(FromRow, FromColumn, ToRow, ToColumn, FromPiece),
  get_next_index_on_same_direction(FromRow, ToRow, CaptureRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, CaptureColumn),
  get_piece_at_position(Board, CaptureRow, CaptureColumn, CapturePiece),
  CapturePiece \== e,
  current_player(Player),
  player_has_piece(Player, FromPiece),
  \+ player_has_piece(Player, CapturePiece),
  get_piece_at_position(Board, ToRow, ToColumn, ToPiece),
  ToPiece == e.


is_valid_capture_queen_piece(Board, FromRow, ToRow, FromColumn, ToColumn, FromPiece, CaptureRow, CaptureColumn) :-
  FromRow =\= ToRow,
  FromColumn =\= ToColumn,
  is_inside_boundaries(ToRow, ToColumn),
  X is abs(ToRow - FromRow),
  Y is abs(ToColumn - FromColumn),
  X =:= Y,
  get_next_index_on_same_direction(FromRow, ToRow, NextRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, NextColumn),
  \+ is_path_free(NextRow, NextColumn, ToRow, ToColumn),
  path_free_last_position_checked(CaptureRow, CaptureColumn),
  get_piece_at_position(Board, CaptureRow, CaptureColumn, CapturePiece),
  current_player(Player),
  player_has_piece(Player, FromPiece),
  \+ player_has_piece(Player, CapturePiece),
  get_next_index_on_same_direction(CaptureRow, ToRow, NextRow2),
  get_next_index_on_same_direction(CaptureColumn, ToColumn, NextColumn2),
  is_path_free(NextRow2, NextColumn2, ToRow, ToColumn),
  get_piece_at_position(Board, ToRow, ToColumn, ToPiece),
  ToPiece == e.


is_valid_capture_diagonals(FromRow, FromColumn, ToRow, ToColumn, r) :-
  is_valid_capture_normal_piece_diagonals(FromRow, FromColumn, ToRow, ToColumn).

is_valid_capture_diagonals(FromRow, FromColumn, ToRow, ToColumn, b) :-
  is_valid_capture_normal_piece_diagonals(FromRow, FromColumn, ToRow, ToColumn).


is_valid_capture_normal_piece_diagonals(FromRow, FromColumn, ToRow, ToColumn) :-
  is_valid_capture_row_normal_piece(FromRow, ToRow),
  is_valid_capture_column_normal_piece(FromColumn, ToColumn).


is_valid_capture_row_normal_piece(FromRow, ToRow) :- ToRow =:= FromRow - 2.
is_valid_capture_row_normal_piece(FromRow, ToRow) :- ToRow =:= FromRow + 2.


is_valid_capture_column_normal_piece(FromColumn, ToColumn) :- ToColumn =:= FromColumn - 2.
is_valid_capture_column_normal_piece(FromColumn, ToColumn) :- ToColumn =:= FromColumn + 2.


is_empty_list([]).


longest_capture_sequence(FromCoord, Sequence) :-
  coord_to_position(FromCoord, FromRow, FromColumn),
  board(Board),
  get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
  findall(Captures, capture_sequence(Board, [FromRow, FromColumn], FromPiece, [], Captures), AllSequences),
  max_sequence(AllSequences, Sequence).


capture_sequence(Board, PiecePos, FromPiece, CurrentSequence, CurrentSequence) :-
  \+ can_capture(Board, PiecePos, FromPiece, _).

capture_sequence(Board, PiecePosition, FromPiece, CurrentSequence, FinalSequence) :-
  can_capture(Board, PiecePosition, FromPiece, NewPosition),
  make_single_capture(Board, PiecePosition, NewPosition, [], NewBoard),
  append(CurrentSequence, [NewPosition], NewCurrentSequence),
  capture_sequence(NewBoard, NewPosition, FromPiece, NewCurrentSequence, FinalSequence).


can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [2, 2], [1, 1], FromPiece, [ToRow, ToColumn]).

can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [2, -2], [1, -1], FromPiece, [ToRow, ToColumn]).

can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [-2, 2], [-1, 1], FromPiece, [ToRow, ToColumn]).

can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [-2, -2], [-1, -1], FromPiece, [ToRow, ToColumn]).


max_sequence([Sequence], Sequence).
max_sequence([Sequence1, Sequence2 | Rest], MaxSequence) :-
  length(Sequence1, Length1),
  length(Sequence2, Length2),
  (Length1 > Length2
    -> max_sequence([Sequence1 | Rest], MaxSequence)
    ; max_sequence([Sequence2 | Rest], MaxSequence)
  ).
