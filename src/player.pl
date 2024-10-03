% Autores
% Gabriel Frasson Costa - 202035001
% Pedro do Couto Filgueiras - 201935015

:- module(player, [
  player/1,
  change_current_player/0,
  player_has_piece/2,
  current_player/1,
  initialize_current_player/1
]).


player(a).
player(b).


player_has_piece(a, r).
player_has_piece(a, rq).
player_has_piece(b, b).
player_has_piece(b, bq).


:- dynamic(current_player/1).


initialize_current_player(InitialPlayer) :-
  retractall(current_player(_)),
  assertz(current_player(InitialPlayer)).


change_current_player() :-
  current_player(a),
  retract(current_player(a)),
  assertz(current_player(b)).

change_current_player() :-
  current_player(b),
  retract(current_player(b)),
  assertz(current_player(a)).
  