% Autores
% Gabriel Frasson Costa - 202035001
% Pedro do Couto Filgueiras - 201935015

:- module(utils, [
  replace_in_list/4,
  replace_position/5,
  get_next_index_on_same_direction/3,
  is_empty_list/1,
  get_head/2,
  get_tail/2,
  get_last_element/2,
  get_element_at_index/3,
  max_sequence/2,
  sort_matrix_by_column/3
]).

:- use_module(game_board).

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


get_next_index_on_same_direction(From, To, Next) :-
  From < To -> Next is From + 1;
  From > To -> Next is From - 1;
  Next = From.


is_empty_list([]).


get_head([Head | _], Head).


get_tail([_ | Tail], Tail).


get_last_element([X], X).
get_last_element([_ | Tail], Last) :- get_last_element(Tail, Last).


get_element_at_index([Element | _], 0, Element).
get_element_at_index([_ | Tail], Index, Element) :- 
  Index > 0,
  NewIndex is Index - 1,
  get_element_at_index(Tail, NewIndex, Element).


max_sequence([Sequence], Sequence).
max_sequence([Sequence1, Sequence2 | Rest], MaxSequence) :-
  length(Sequence1, Length1),
  length(Sequence2, Length2),
  (Length1 > Length2
    -> max_sequence([Sequence1 | Rest], MaxSequence)
    ; max_sequence([Sequence2 | Rest], MaxSequence)
  ).


sort_matrix_by_column(Matrix, Index, SortedMatrix) :-
  get_pairs_from_column(Matrix, Index, Pairs),
  keysort(Pairs, SortedPairs),
  pairs_values(SortedPairs, SortedMatrix).


get_pairs_from_column([], _, []).
get_pairs_from_column([Row | Rows], Index, [Value-Row | Pairs]) :-
  nth0(Index, Row, Value),
  get_pairs_from_column(Rows, Index, Pairs).


pairs_values([], []).
pairs_values([_-Value | Tail], [Value | ValuesTail]) :- pairs_values(Tail, ValuesTail).