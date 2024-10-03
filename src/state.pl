% Autores
% Gabriel Frasson Costa - 202035001
% Pedro do Couto Filgueiras - 201935015

:- module(state, [
  display_game_state/0,
  update_state/0,
  update_state/1
]).


:- use_module(player).
:- use_module(game_board).


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


update_state(capture) :-
  check_end_game(Win),
  change_current_player(),
  display_game_state(),
  (Win == false -> (
    write('Digite a sua ação: '), nl
  )).


check_end_game(Win) :-
  current_player(Player),

  (Player == a -> (
    OtherPlayerNormalPiece = b,
    OtherPlayerQueenPiece = bq
  ) ; (
    OtherPlayerNormalPiece = r,
    OtherPlayerQueenPiece = rq
  )),

  find_piece_positions(OtherPlayerNormalPiece, ComputerPiecePositions),
  find_piece_positions(OtherPlayerQueenPiece, ComputerQueenPiecePositions),
  append(ComputerPiecePositions, ComputerQueenPiecePositions, FromPositions),
  
  length(FromPositions, LengthFromPositions),

  (LengthFromPositions =:= 0
    -> (
        write('O jogador '),
        write(Player),
        write(' venceu!!!'), nl,
        Win = true
    ) ; Win = false
  ).