% Autores
% Gabriel Frasson Costa - 202035001
% Pedro do Couto Filgueiras - 201935015

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
  write('Jogo de Damas!'), nl,
  write('Para mover uma peça, execute o comando: mv(COORD1, COORD2).'), nl,
  write('Para capturar uma ou mais peças, execute o comando: cap(COORD1, [COORD2, COORD3,..., COORDN]).'), nl,
  write('Para executar uma jogada feita pelo computador, execute o comando computer_move().'), nl,
  nl,
  choose_initial_player(Player),
  initialize_current_player(Player),
  initialize_board(),
  display_game_state(),
  write('Digite a sua ação: '),
  write('\n').
