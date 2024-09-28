:- module(game_board, [
  board/1,
  initialize_board/0,
  display_board/0,
  coord_to_position/3,
  replace_position/5,
  get_piece_at_position/4
]).

:- dynamic(board/1).


initialize_board :-
  initial_board(Board),
  retractall(board(_)),
  assertz(board(Board)).


initial_board([
  [e, b, e, b, e, b, e, b],
  [b, e, b, e, b, e, b, e],
  [e, b, e, b, e, b, e, b],
  [e, e, e, e, e, e, e, e],
  [e, e, e, e, e, e, e, e],
  [w, e, w, e, w, e, w, e],
  [e, w, e, w, e, w, e, w],
  [w, e, w, e, w, e, w, e]
]).


display_board() :-
  board(Board),
  display_matrix(Board).


display_matrix([]).
display_matrix([H | T]) :-
  write(H),
  write('\n'),
  display_matrix(T).


% replace_position(List, Position, Value, NewList)
replace_in_list([_ | T], 0, Value, [Value | T]).
replace_in_list([H | T], Position, Value, [H | T1]) :-
  Position > 0,
  Position1 is Position - 1,
  replace_in_list(T, Position1, Value, T1).


% replace_position(Board, Row, Column, Value, NewBoard)
replace_position([H | T], 0, Column, Value, [NewList | T]) :- replace_in_list(H, Column, Value, NewList).
replace_position([H | T], Row, Column, Value, [H | T1]) :-
  Row > 0,
  Row1 is Row - 1,
  replace_position(T, Row1, Column, Value, T1).


% get_piece_at_position(Board, Row, Column, Piece)
get_piece_at_position([H | _], 0, Column, Piece) :- nth0(Column, H, Piece).
get_piece_at_position([_ | T], Row, Column, Piece) :-
  Row > 0,
  Row1 is Row - 1,
  get_piece_at_position(T, Row1, Column, Piece).


column_to_index(a, 0).
column_to_index(b, 1).
column_to_index(c, 2).
column_to_index(d, 3).
column_to_index(e, 4).
column_to_index(f, 5).
column_to_index(g, 6).
column_to_index(h, 7).


coord_to_position(Coord, Row, Column) :-
  atom_chars(Coord, [ColumnLetter, RowChar]),
  column_to_index(ColumnLetter, Column),
  atom_number(RowChar, RowAux),
  Row is 8 - RowAux.
