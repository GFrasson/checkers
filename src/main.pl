:- use_module(game_board).
:- use_module(player).
:- use_module(move).
:- use_module(capture).
:- use_module(computer).


choose_initial_player(Player) :-
  write('Digite o jogador inicial (a. / b.): '),
  read(PlayerAux),
  player(PlayerAux) -> Player = PlayerAux ; choose_initial_player(Player).


start_game() :-
  write('Jogo de Damas!\n'),
  nl,
  choose_initial_player(Player),
  initialize_current_player(Player),
  initialize_board(),
  display_game_state(),
  write('Digite a sua ação: '),
  write('\n').
