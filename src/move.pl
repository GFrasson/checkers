:- module(move, [mv/2]).

:- use_module(game_board).


mv(COORD1, COORD2) :-
  board(Board),
  coord_to_position(COORD1, Row1, Column1),
  coord_to_position(COORD2, Row2, Column2),
  make_move(Board, Row1, Column1, Row2, Column2, NewBoard),
  retract(board(Board)),
  assertz(board(NewBoard)).


make_move(Board, FromRow, FromColumn, ToRow, ToColumn, NewBoard) :-
  get_piece_at_position(Board, FromRow, FromColumn, Piece),
  replace_position(Board, FromRow, FromColumn, e, NewBoardAux),
  replace_position(NewBoardAux, ToRow, ToColumn, Piece, NewBoard).
  

% cap(COORD1, [COORD2 | COORDS]).