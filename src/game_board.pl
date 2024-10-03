% Autores
% Gabriel Frasson Costa - 202035001
% Pedro do Couto Filgueiras - 201935015

:- module(game_board, [
  board/1,
  initialize_board/0,
  display_board/0,
  display_board/1,
  coord_to_position/3,
  position_to_coord/2,
  get_piece_at_position/4,
  promotion_piece/2,
  is_inside_boundaries/2,
  is_promotion/2,
  queen/1,
  is_path_free/4,
  path_free_last_position_checked/2,
  find_piece_positions/2,
  piece_value/2
]).


:- use_module(utils).


:- dynamic(board/1).


piece(r).
piece(b).
piece(rq).
piece(bq).
piece(e).


queen(rq).
queen(bq).


promotion_piece(r, rq).
promotion_piece(b, bq).


piece_value(b, 2).
piece_value(bq, 9).
piece_value(r, -2).
piece_value(rq, -9).
piece_value(e, 0).


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
  [r, e, r, e, r, e, r, e],
  [e, r, e, r, e, r, e, r],
  [r, e, r, e, r, e, r, e]
]).


is_inside_boundaries(Row, Column) :-
  Row >= 0,
  Row =< 7,
  Column >= 0,
  Column =< 7.


is_promotion(r, ToRow) :- ToRow =:= 0.
is_promotion(b, ToRow) :- ToRow =:= 7.


display_board(Board) :-
  display_board_matrix(Board, 0),
  write('   A  B  C  D  E  F  G  H \n').

display_board() :-
  board(Board),
  display_board_matrix(Board, 0),
  write('   A  B  C  D  E  F  G  H \n').


display_board_matrix([], _).
display_board_matrix([H | T], Index) :-
  Coordinate is 8 - Index,
  write(Coordinate),
  write(' '),
  display_board_row(H, Index),
  write('\n'),
  NextIndex is Index + 1,
  display_board_matrix(T, NextIndex).


display_board_row([], _).
display_board_row([H | T], Index) :-
  piece(H),
  get_display_background(Index, Background),
  display_piece(H, Background),
  NextIndex is Index + 1,
  display_board_row(T, NextIndex).


display_piece(b, Background) :- ansi_format([fg(cyan), Background], ' O ', []).
display_piece(r, Background) :- ansi_format([fg(red), Background], ' O ', []).
display_piece(bq, Background) :- ansi_format([underline, bold, fg(cyan), Background], ' O ', []).
display_piece(rq, Background) :- ansi_format([underline, bold, fg(red), Background], ' O ', []).
display_piece(e, Background) :- ansi_format([Background], '   ', []).


get_display_background(Index, Background) :-
  Index mod 2 =:= 0,
  Background = bg(white).

get_display_background(Index, Background) :-
  Index mod 2 =\= 0,
  Background = bg(black).


% get_piece_at_position(Board, Row, Column, Piece)
get_piece_at_position([H | _], 0, Column, Piece) :- nth0(Column, H, Piece).
get_piece_at_position([_ | T], Row, Column, Piece) :-
  Row > 0,
  Row1 is Row - 1,
  get_piece_at_position(T, Row1, Column, Piece).


find_piece_positions(Piece, Positions) :-
  findall([RowIndex, ColIndex], find_piece_position(Piece, RowIndex, ColIndex), Positions).


find_piece_position(Piece, RowIndex, ColIndex) :-
  board(Board),
  nth0(RowIndex, Board, Row),
  nth0(ColIndex, Row, Piece).


column_to_index(a, 0).
column_to_index(b, 1).
column_to_index(c, 2).
column_to_index(d, 3).
column_to_index(e, 4).
column_to_index(f, 5).
column_to_index(g, 6).
column_to_index(h, 7).

index_to_column(0, a).
index_to_column(1, b).
index_to_column(2, c).
index_to_column(3, d).
index_to_column(4, e).
index_to_column(5, f).
index_to_column(6, g).
index_to_column(7, h).


coord_to_position(Coord, Row, Column) :-
  atom_chars(Coord, [ColumnLetter, RowChar]),
  column_to_index(ColumnLetter, Column),
  atom_number(RowChar, RowAux),
  Row is 8 - RowAux.


position_to_coord([Row, Column], Coord) :-
  index_to_column(Column, ColumnLetter),
  RowAux is 8 - Row,
  atom_number(RowChar, RowAux),
  atom_chars(Coord, [ColumnLetter, RowChar]).


:- dynamic(path_free_last_position_checked/2).


is_path_free(FromRow, FromColumn, FromRow, FromColumn) :-
  % write('base case: '), write(FromRow), write(','), write(FromColumn), nl,
  retractall(path_free_last_position_checked(_, _)),
  assertz(path_free_last_position_checked(FromRow, FromColumn)),
  board(Board),
  get_piece_at_position(Board, FromRow, FromColumn, Piece),
  Piece == e.

is_path_free(FromRow, FromColumn, ToRow, ToColumn) :-
  % write('Checking position: '), write(FromRow), write(','), write(FromColumn), nl,
  retractall(path_free_last_position_checked(_, _)),
  assertz(path_free_last_position_checked(FromRow, FromColumn)),
  board(Board),
  get_piece_at_position(Board, FromRow, FromColumn, Piece),
  Piece == e,
  get_next_index_on_same_direction(FromRow, ToRow, NextRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, NextColumn),

  ((NextRow =\= FromRow ; NextColumn =\= FromColumn)
    -> is_path_free(NextRow, NextColumn, ToRow, ToColumn)
    ; fail
  ).
