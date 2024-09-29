:- module(move, [mv/2]).

:- use_module(game_board).
:- use_module(player).


mv(COORD1, COORD2) :-
  board(Board),
  current_player(Player),
  coord_to_position(COORD1, Row1, Column1),
  coord_to_position(COORD2, Row2, Column2),
  make_move(Board, Row1, Column1, Row2, Column2, Player, NewBoard),
  retract(board(Board)),
  assertz(board(NewBoard)).


make_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, NewBoard) :-
  get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
  is_valid_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, FromPiece),
  replace_position(Board, FromRow, FromColumn, e, NewBoardAux),
  replace_position(NewBoardAux, ToRow, ToColumn, FromPiece, NewBoard).


is_valid_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, FromPiece) :-
  get_piece_at_position(Board, ToRow, ToColumn, ToPiece),
  ToPiece =:= e,
  player_has_piece(Player, FromPiece),
  is_valid_move_for_piece(FromRow, FromColumn, ToRow, ToColumn, FromPiece).


is_valid_move_for_piece(FromRow, FromColumn, ToRow, ToColumn, r) :-
  is_valid_move_red(FromRow, FromColumn, ToRow, ToColumn).

is_valid_move_for_piece(FromRow, FromColumn, ToRow, ToColumn, b) :-
  is_valid_move_blue(FromRow, FromColumn, ToRow, ToColumn).

is_valid_move_for_piece(FromRow, FromColumn, ToRow, ToColumn, rq) :-
  is_valid_move_queen(FromRow, FromColumn, ToRow, ToColumn).

is_valid_move_for_piece(FromRow, FromColumn, ToRow, ToColumn, bq) :-
  is_valid_move_queen(FromRow, FromColumn, ToRow, ToColumn).

is_valid_move_red(FromRow, FromColumn, ToRow, ToColumn) :-
  ToRow =:= FromRow - 1,
  ToColumn =:= FromColumn - 1.

is_valid_move_red(FromRow, FromColumn, ToRow, ToColumn) :-
  ToRow =:= FromRow - 1,
  ToColumn =:= FromColumn + 1.


is_valid_move_blue(FromRow, FromColumn, ToRow, ToColumn) :-
  ToRow =:= FromRow + 1,
  ToColumn =:= FromColumn - 1.

is_valid_move_blue(FromRow, FromColumn, ToRow, ToColumn) :-
  ToRow =:= FromRow + 1,
  ToColumn =:= FromColumn + 1.


is_valid_move_queen(FromRow, FromColumn, ToRow, ToColumn) :-
  X is abs(ToRow - FromRow),
  Y is abs(ToColumn - FromColumn),
  X =:= Y.


% cap(COORD1, [COORD2 | COORDS]).