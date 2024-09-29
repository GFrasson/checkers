:- module(capture, [
  cap/2
]).

:- use_module(game_board).
:- use_module(player).


cap(COORD1, [COORD2 | COORDS]).
