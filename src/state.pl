:- module(state, [
  display_game_state/0,
  update_state/0
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
  