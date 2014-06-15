-module(game_test).
-author("Alex").

-include_lib("eunit/include/eunit.hrl").
-define(NEW_GAME, game:create(1,2)).
-define(LEFT_TOP_WHITE, {2,6}).
-define(LEFT_2ND_LINE_WHITE, {1,7}).
-define(LEFT_BOTTOM_BLACK, {1,3}).

whose_turn_after_create_test() ->
  Game = ?NEW_GAME,
  ?assertMatch({white, _}, game:whose_turn(Game)).

dump_after_create_test() ->
  Game = ?NEW_GAME,
  ?assertEqual([
    "*.*.*.*.",
    ".*.*.*.*",
    "*.*.*.*.",
    "........",
    "........",
    ".o.o.o.o",
    "o.o.o.o.",
    ".o.o.o.o"
  ], game:dump(Game)).

player_after_create_test() ->
  Game = ?NEW_GAME,
  ?assertEqual({white, 1}, game:player(Game, white)),
  ?assertEqual({black, 2}, game:player(Game, black)),
  ?assertEqual([
    {white, 1},
    {black, 2}
  ], game:players(Game)).

winner_after_create_test() ->
  Game = ?NEW_GAME,
  ?assertEqual(false, game:winner(Game)).

to({X, Y}, Direction) ->
  case Direction of
    l -> {X-1,Y};
    r -> {X+1,Y};
    u -> {X,Y-1};
    d -> {X,Y+1};
    [H|T] -> to(to({X,Y}, H), T);
    [] -> {X,Y}
  end.

-define(MOVE(Game, Item, Direction), game:move(Game, Item, to(Item, Direction))).



first_turn_white_valid_test() ->
  {Result, Game} = ?MOVE(?NEW_GAME, ?LEFT_TOP_WHITE, [u,l]),
  ?assertEqual(ok, Result),
  ?assertEqual([
    "*.*.*.*.",
    ".*.*.*.*",
    "*.*.*.*.",
    "........",
    "o.......",
    "...o.o.o",
    "o.o.o.o.",
    ".o.o.o.o"
  ], game:dump(Game)).



first_turn_white_invalid_forward_test() ->
  ?assertMatch({error, rules_direction}, ?MOVE(?NEW_GAME, ?LEFT_TOP_WHITE, u)).

first_turn_white_invalid_jump_test() ->
  ?assertMatch({error, rules_jump}, ?MOVE(?NEW_GAME, ?LEFT_TOP_WHITE, [u,r,u,r])).


first_turn_white_invalid_backward_test() ->
  From = ?LEFT_TOP_WHITE,
  To = to(From, [u,l]),
  {ok, Game1} = game:move(?NEW_GAME, From, To),
  {ok, Game2} = ?MOVE(Game1, ?LEFT_BOTTOM_BLACK, [d,r]),
  ?assertMatch({error, rules_backward}, ?MOVE(Game2, To, [d,r])).

first_turn_white_invalid_try_to_move_black_test() ->
  ?assertMatch({error, rules_opposite_from}, ?MOVE(?NEW_GAME, ?LEFT_BOTTOM_BLACK, [d,r])).

first_turn_white_invalid_try_to_move_empty_test() ->
  ?assertMatch({error, rules_empty_from}, ?MOVE(?NEW_GAME, {5, 5}, [u,l])).

first_turn_white_invalid_try_to_jump_over_own_test() ->
  ?assertMatch({error, rules_jump_over_self}, ?MOVE(?NEW_GAME, ?LEFT_2ND_LINE_WHITE, [u,u,r,r])).

first_turn_white_valid_whose_turn_test() ->
  {ok, Game1} = ?MOVE(?NEW_GAME, ?LEFT_TOP_WHITE, [u,l]),
  ?assertEqual({black, 2}, game:whose_turn(Game1)).

second_turn_black_valid_test() ->
  {ok, Game1} = ?MOVE(?NEW_GAME, ?LEFT_TOP_WHITE, [u,l]),
  {Res, Gam2} = ?MOVE(Game1, ?LEFT_BOTTOM_BLACK, [d,r]),
  ?assertMatch({ok, _}, {Res, Gam2}),
  ?assertEqual([
    "*.*.*.*.",
    ".*.*.*.*",
    "..*.*.*.",
    ".*......",
    "o.......",
    "...o.o.o",
    "o.o.o.o.",
    ".o.o.o.o"
  ], game:dump(Gam2)).

single_eating_test() ->
  {ok, Game1} = ?MOVE(?NEW_GAME, {4, 6}, [u, l]),
  {ok, Game2} = ?MOVE(Game1, ?LEFT_BOTTOM_BLACK, [d, r]),
  {ok, Game3} = ?MOVE(Game2, {3, 5}, [u,l,u,l]),
  ?assertEqual([
    "*.*.*.*.",
    ".*.*.*.*",
    "o.*.*.*.",
    "........",
    "........",
    ".o...o.o",
    "o.o.o.o.",
    ".o.o.o.o"
  ], game:dump(Game3)).