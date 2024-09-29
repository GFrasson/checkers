:- module(capture, [
  cap/2
]).

:- use_module(game_board).
:- use_module(player).
:- use_module(move).


cap(_, []).
cap(COORD1, [COORD2 | COORDS]) :-
  board(Board),
  current_player(Player),
  make_capture(Board, COORD1, [COORD2 | COORDS], Player, NewBoard),
  retractall(board(_)),
  assertz(board(NewBoard)),
  update_state().


make_capture(Board, _, [], _, Board).
make_capture(Board, COORD1, [COORD2 | COORDS], Player, NewBoard) :-
  coord_to_position(COORD1, FromRow, FromColumn),
  coord_to_position(COORD2, ToRow, ToColumn),
  get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
  is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, Player, FromPiece, CaptureRow, CaptureColumn),
  replace_position(Board, FromRow, FromColumn, e, NewBoardAux),
  replace_position(NewBoardAux, CaptureRow, CaptureColumn, e, NewBoardAux2),
  ((is_empty_list(COORDS), is_promotion(FromPiece, ToRow)) -> (
      promotion_piece(FromPiece, PromotionPiece),
      replace_position(NewBoardAux2, ToRow, ToColumn, PromotionPiece, NewBoardUpdated)
    )
    ; replace_position(NewBoardAux2, ToRow, ToColumn, FromPiece, NewBoardUpdated)
  ),
  make_capture(NewBoardUpdated, COORD2, COORDS, Player, NewBoard).


is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, Player, FromPiece, CaptureRow, CaptureColumn) :-
  FromRow =\= ToRow,
  FromColumn =\= ToColumn,
  is_inside_boundaries(ToRow, ToColumn),
  is_valid_capture_for_piece(FromRow, FromColumn, ToRow, ToColumn, FromPiece),
  get_next_index_on_same_direction(FromRow, ToRow, CaptureRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, CaptureColumn),
  get_piece_at_position(Board, CaptureRow, CaptureColumn, CapturePiece),
  CapturePiece \== e,
  player_has_piece(Player, FromPiece),
  \+ player_has_piece(Player, CapturePiece),
  get_piece_at_position(Board, ToRow, ToColumn, ToPiece),
  ToPiece == e.


is_valid_capture_for_piece(FromRow, FromColumn, ToRow, ToColumn, r) :-
  is_valid_capture_normal_piece(FromRow, FromColumn, ToRow, ToColumn).

is_valid_capture_for_piece(FromRow, FromColumn, ToRow, ToColumn, b) :-
  is_valid_capture_normal_piece(FromRow, FromColumn, ToRow, ToColumn).
  

is_valid_capture_normal_piece(FromRow, FromColumn, ToRow, ToColumn) :-
  is_valid_capture_row_normal_piece(FromRow, ToRow),
  is_valid_capture_column_normal_piece(FromColumn, ToColumn).


is_valid_capture_row_normal_piece(FromRow, ToRow) :- ToRow =:= FromRow - 2.
is_valid_capture_row_normal_piece(FromRow, ToRow) :- ToRow =:= FromRow + 2.


is_valid_capture_column_normal_piece(FromColumn, ToColumn) :- ToColumn =:= FromColumn - 2.
is_valid_capture_column_normal_piece(FromColumn, ToColumn) :- ToColumn =:= FromColumn + 2.


is_empty_list([]).