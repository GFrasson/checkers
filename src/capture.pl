:- module(capture, [
  cap/2,
  longest_capture_sequence/3
]).

:- use_module(game_board).
:- use_module(player).
:- use_module(move).


cap(_, []).
cap(COORD1, [COORD2 | COORDS]) :-
  board(Board),
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


is_valid_capture(Board, FromRow, ToRow, FromColumn, ToColumn, FromPiece, CaptureRow, CaptureColumn) :-
  FromRow =\= ToRow,
  FromColumn =\= ToColumn,
  is_inside_boundaries(ToRow, ToColumn),
  is_valid_capture_for_piece(FromRow, FromColumn, ToRow, ToColumn, FromPiece),
  get_next_index_on_same_direction(FromRow, ToRow, CaptureRow),
  get_next_index_on_same_direction(FromColumn, ToColumn, CaptureColumn),
  get_piece_at_position(Board, CaptureRow, CaptureColumn, CapturePiece),
  CapturePiece \== e,
  current_player(Player),
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


% Predicate to find the longest capture sequence
longest_capture_sequence(FromRow, FromColumn, Sequence) :-
  board(Board),
  get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
  findall(Captures, capture_sequence(Board, [FromRow, FromColumn], FromPiece, [], Captures), AllSequences),
  max_sequence(AllSequences, Sequence).

% Base case: no more captures possible
capture_sequence(Board, PiecePos, FromPiece, CurrentSequence, CurrentSequence) :-
  \+ can_capture(Board, PiecePos, FromPiece, _).

% Recursive case: perform capture and continue searching for more captures
capture_sequence(Board, PiecePos, FromPiece, CurrentSequence, FinalSequence) :-
  can_capture(Board, PiecePos, FromPiece, NewPos),
  make_single_capture(Board, PiecePos, NewPos, [], NewBoard),
  capture_sequence(NewBoard, NewPos, FromPiece, [NewPos|CurrentSequence], FinalSequence).

% Predicate to check if a capture is possible
% Logic to check if a capture is possible in any diagonal 
can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [2, 2], [1, 1], FromPiece, [ToRow, ToColumn]).

can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [2, -2], [1, -1], FromPiece, [ToRow, ToColumn]).

can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [-2, 2], [-1, 1], FromPiece, [ToRow, ToColumn]).

can_capture(Board, [FromRow, FromColumn], FromPiece, [ToRow, ToColumn]) :-
  is_valid_capture(Board, FromRow, FromColumn, [-2, -2], [-1, -1], FromPiece, [ToRow, ToColumn]).


% Predicate to find the sequence with the maximum length
max_sequence([Seq], Seq).
max_sequence([Seq1, Seq2 | Rest], MaxSeq) :-
  length(Seq1, Len1),
  length(Seq2, Len2),
  ( Len1 > Len2 -> max_sequence([Seq1|Rest], MaxSeq)
  ; max_sequence([Seq2|Rest], MaxSeq) ).
