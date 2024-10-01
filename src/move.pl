:- module(move, [
  mv/2,
  display_game_state/0,
  get_next_index_on_same_direction/3,
  update_state/0,
  is_path_free/4,
  path_free_last_position_checked/2
]).

:- use_module(game_board).
:- use_module(player).


mv(COORD1, COORD2) :-
  board(Board),
  current_player(Player),
  coord_to_position(COORD1, Row1, Column1),
  coord_to_position(COORD2, Row2, Column2),
  make_move(Board, Row1, Column1, Row2, Column2, Player, NewBoard),
  retract(board(Board)),
  assertz(board(NewBoard)),
  update_state().


make_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, NewBoard) :-
  get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
  is_valid_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, FromPiece),
  replace_position(Board, FromRow, FromColumn, e, NewBoardAux),
  (is_promotion(FromPiece, ToRow) -> (
      promotion_piece(FromPiece, PromotionPiece),
      replace_position(NewBoardAux, ToRow, ToColumn, PromotionPiece, NewBoard)
    )
    ; replace_position(NewBoardAux, ToRow, ToColumn, FromPiece, NewBoard)
  ).


is_valid_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, FromPiece) :-
  FromRow =\= ToRow,
  FromColumn =\= ToColumn,
  is_inside_boundaries(ToRow, ToColumn),
  get_piece_at_position(Board, ToRow, ToColumn, ToPiece),
  ToPiece == e,
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
  X =:= Y,
  get_next_index_on_same_direction(FromRow, ToRow, NextRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, NextColumn),
  is_path_free(NextRow, NextColumn, ToRow, ToColumn).


:- dynamic(path_free_last_position_checked/2).

is_path_free(FromRow, FromColumn, FromRow, FromColumn) :-
  retractall(path_free_last_position_checked(_, _)),
  assertz(path_free_last_position_checked(FromRow, FromColumn)),
  board(Board),
  get_piece_at_position(Board, FromRow, FromColumn, Piece),
  Piece == e.


is_path_free(FromRow, FromColumn, ToRow, ToColumn) :-
  retractall(path_free_last_position_checked(_, _)),
  assertz(path_free_last_position_checked(FromRow, FromColumn)),
  board(Board),
  get_piece_at_position(Board, FromRow, FromColumn, Piece),
  Piece == e,
  get_next_index_on_same_direction(FromRow, ToRow, NextRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, NextColumn),
  is_path_free(NextRow, NextColumn, ToRow, ToColumn).


get_next_index_on_same_direction(From, To, Next) :-
  (To > From -> Next is From + 1 ; Next is From - 1).
  

display_game_state() :-
  current_player(Player),
  write('Jogador atual: '),
  write(Player),
  write('\n'),
  display_board().


update_state() :-
  change_current_player(),
  display_game_state(),
  write('Digite a sua ação: '),
  write('\n').
