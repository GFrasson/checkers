:- use_module(game_board).
:- use_module(player).
:- use_module(move).


choose_initial_player(Player) :-
  write('Digite o jogador inicial (a. / b.): '),
  read(PlayerAux),
  player(PlayerAux) -> Player = PlayerAux ; choose_initial_player(Player).


start_game() :-
  write('Jogo de Damas!\n'),
  choose_initial_player(Player),
  initialize_board(),
  display_board,
  game_event_loop(Player).


game_event_loop(Player) :-
  write('Digite a sua ação: '),
  write('\n'),
  read(Action),
  call(Action),
  display_board,
  change_current_player(Player, NewPlayer),
  game_event_loop(NewPlayer).
