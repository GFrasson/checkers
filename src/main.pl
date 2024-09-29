:- use_module(game_board).
:- use_module(player).
:- use_module(move).
:- use_module(capture).


choose_initial_player(Player) :-
  write('Digite o jogador inicial (a. / b.): '),
  read(PlayerAux),
  player(PlayerAux) -> Player = PlayerAux ; choose_initial_player(Player).


start_game2() :-
  write('Jogo de Damas!\n'),
  choose_initial_player(Player),
  initialize_current_player(Player),
  initialize_board(),
  display_board,
  game_event_loop().


game_event_loop() :-
  write('Digite a sua ação: '),
  write('\n'),
  read(Action),
  call_action(Action),
  display_board,
  change_current_player(),
  game_event_loop().


call_action(Action) :-
  callable(Action),
  compound(Action),
  functor(Action, mv, 2),
  call(Action).

call_action(Action) :-
  callable(Action),
  compound(Action),
  functor(Action, cap, 2),
  call(Action).


start_game() :-
  write('Jogo de Damas!\n'),
  choose_initial_player(Player),
  initialize_current_player(Player),
  initialize_board(),
  display_game_state(),
  write('Digite a sua ação: '),
  write('\n').
