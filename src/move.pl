:- module(move, [
  mv/2,
  display_game_state/0,
  get_next_index_on_same_direction/3,
  is_path_free/4,
  path_free_last_position_checked/2,
  get_all_valid_moves/4,
  make_move/7
]).

:- use_module(game_board).
:- use_module(player).
:- use_module(utils).
:- use_module(state).


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


get_all_valid_moves(FromRow, FromColumn, Piece, DestinationPositions) :-
  (queen(Piece)
    -> get_all_valid_moves_queen_piece(FromRow, FromColumn, DestinationPositions)
    ; get_all_valid_moves_normal_piece(FromRow, FromColumn, DestinationPositions)
  ).


get_all_valid_moves_normal_piece(FromRow, FromColumn, DestinationPositions) :-
  board(Board),
  current_player(Player),
  Deltas = [[1, 1], [1, -1], [-1, 1], [-1, -1]],

  findall(
    [ToRow, ToColumn],
    (
      member(Delta, Deltas),
      get_destination_position([FromRow, FromColumn], Delta, [ToRow, ToColumn]),
      get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
      is_valid_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, FromPiece)
    ),
    DestinationPositions
  ).


get_destination_position([FromRow, FromColumn], [DeltaRow, DeltaColumn], [ToRow, ToColumn]) :-
  ToRow is FromRow + DeltaRow,
  ToColumn is FromColumn + DeltaColumn.


get_all_valid_moves_queen_piece(FromRow, FromColumn, DestinationPositions) :-
  get_all_valid_moves_queen_piece_delta(FromRow, FromColumn, [1, 1], DestinationPositions1),
  get_all_valid_moves_queen_piece_delta(FromRow, FromColumn, [1, -1], DestinationPositions2),
  get_all_valid_moves_queen_piece_delta(FromRow, FromColumn, [-1, 1], DestinationPositions3),
  get_all_valid_moves_queen_piece_delta(FromRow, FromColumn, [-1, -1], DestinationPositions4),

  append(DestinationPositions1, DestinationPositions2, DestinationPositionsAux),
  append(DestinationPositionsAux, DestinationPositions3, DestinationPositionsAux2),
  append(DestinationPositionsAux2, DestinationPositions4, DestinationPositions).


get_all_valid_moves_queen_piece_delta(FromRow, FromColumn, [DeltaRow, DeltaColumn], DestinationPositions) :-
  board(Board),
  current_player(Player),
  Indexes = [1, 2, 3, 4, 5, 6, 7],

  findall(
    [ToRow, ToColumn],
    (
      member(Index, Indexes),
      NewDeltaRow is DeltaRow * Index,
      NewDeltaColumn is DeltaColumn * Index,
      get_destination_position([FromRow, FromColumn], [NewDeltaRow, NewDeltaColumn], [ToRow, ToColumn]),
      get_piece_at_position(Board, FromRow, FromColumn, FromPiece),
      is_valid_move(Board, FromRow, FromColumn, ToRow, ToColumn, Player, FromPiece)
    ),
    DestinationPositions
  ).
