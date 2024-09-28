:- module(player, [
  player/1,
  change_current_player/2
]).


player(a).
player(b).


change_current_player(a, b).
change_current_player(b, a).
