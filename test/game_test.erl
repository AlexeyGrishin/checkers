-module(game_test).
-author("Alex").

-include_lib("eunit/include/eunit.hrl").
-include("../src/checkers/game_state.hrl").

-define(NEW_GAME, game:create(1,2)).
-define(LEFT_TOP_WHITE, {2,6}).
-define(LEFT_2ND_LINE_WHITE, {1,7}).
-define(LEFT_BOTTOM_BLACK, {1,3}).
-define(WHITE_QUEEN, {5,5}).

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
-define(JUMP(Game, Item, Direction), game:move(Game, Item, to(Item, Direction ++ Direction))).



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
  ?assertMatch({error, rules_jump_over_own}, ?MOVE(?NEW_GAME, ?LEFT_2ND_LINE_WHITE, [u,u,r,r])).

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

create_from_cells_test() ->
  Game = game:create(1, 2, #game_state{
  checkers = [
    #checker{x = 1,y = 1, side = black},
    #checker{x = 2,y = 2, side = white}
  ], whose_turn = black}),
  ?assertMatch({black, _}, game:whose_turn(Game)),
  ?assertMatch(false, game:winner(Game)),
  ?assertEqual([
    "*.......",
    ".o......",
    "........",
    "........",
    "........",
    "........",
    "........",
    "........"
  ], game:dump(Game)).

cells(Cells, WhosTurn) ->
  #game_state{
    whose_turn = WhosTurn,
    checkers = lists:map(
    fun
      ({X, Y, Side}) -> #checker{x = X, y = Y, side = Side};
      ({X, Y, Side, Type}) -> #checker{x = X, y = Y, side = Side, type = Type}
    end, Cells
  )}.


create_from_cells_win_test() ->
  Game = game:create(1, 2, cells([{1,1,black}], white)),
  ?assertMatch({black, _}, game:winner(Game)).

move_to_win_test() ->
  Game = game:create(1, 2, cells([
    {1,1, black},
    {2,2, white}
  ], black)),
  ?assertEqual(false, game:winner(Game)),
  {ok, Game2} = ?JUMP(Game, {1,1}, [r,d]),
  ?assertMatch({black, _}, game:winner(Game2)).

move_after_win_test() ->
  Game = game:create(1, 2, cells([
    {1,1, black},
    {2,2, white}
  ], black)),
  ?assertEqual(false, game:winner(Game)),
  {ok, Game2} = ?JUMP(Game, {1,1}, [r,d]),
  ?assertMatch({error, state_gameover}, game:move(Game2, {3,3}, {4,4})).


move_outside_top_test() ->
  Game = game:create(1, 2, cells([
    {1,1, white},
    {2,2, black}
  ], white)),
  ?assertMatch({error, rules_outside}, ?MOVE(Game, {1,1}, [u,l])),
  ?assertMatch({error, rules_outside}, ?MOVE(Game, {1,1}, [u,r])).

move_outside_left_test() ->
  Game = game:create(1, 2, cells([
    {1,3, white},
    {7,7, black}
  ], white)),
  ?assertMatch({error, rules_outside}, ?MOVE(Game, {1,3}, [u,l])),
  ?assertMatch({ok, _}, ?MOVE(Game, {1,3}, [u,r])).

move_outside_bottom_test() ->
  Game = game:create(1, 2, cells([
    {1,1, white},
    {8,8, black}
  ], black)),
  ?assertMatch({error, rules_outside}, ?MOVE(Game, {8,8}, [d,l])),
  ?assertMatch({error, rules_outside}, ?MOVE(Game, {8,8}, [d,r])).

move_outside_right_test() ->
  Game = game:create(1, 2, cells([
    {1,1, white},
    {8,5, black}
  ], black)),
  ?assertMatch({error, rules_outside}, ?MOVE(Game, {8,5}, [d,r])),
  ?assertMatch({ok, _}, ?MOVE(Game, {8,5}, [d,l])).


get_state_test() ->
  Game = game:create(1, 2, cells([
    {1,1,white},
    {2,2,black}
  ], black)),
  ?assertEqual(#game_state{checkers = [
    #checker{x=1,y=1,side=white,type=simple},
    #checker{x=2,y=2,side=black,type=simple}
  ], whose_turn = black}, game:get_state(Game)).

queen_move_1_step_test() ->
  Game = game:create(1, 2, cells([
    {5,5,white,queen},
    {1,1,black}
  ], white)),
  ?assertMatch({ok, _}, ?MOVE(Game, ?WHITE_QUEEN, [d,l])).

queen_move_2_step_test() ->
  Game = game:create(1, 2, cells([
    {5,5,white,queen},
    {1,1,black}
  ], white)),
  ?assertMatch({ok, _}, ?JUMP(Game, ?WHITE_QUEEN, [u,r])).

queen_eat_1_enemy_test() ->
  Game = game:create(1, 2, cells([
    {5,5,white,queen},
    {3,3,black}
  ], white)),
  {Res, Game2} =  game:move(Game, ?WHITE_QUEEN, {2,2}),
  ?assertEqual(ok, Res),
  ?assertMatch({white, _}, game:winner(Game2)).

queen_eat_2_enemies_test() ->
  Game = game:create(1, 2, cells([
    {5,5,white,queen},
    {3,3,black},
    {4,4,black}
  ], white)),
  {Res, Game2} =  game:move(Game, ?WHITE_QUEEN, {2,2}),
  ?assertEqual(ok, Res),
  ?assertMatch({white, _}, game:winner(Game2)).

queen_jump_over_own_test() ->
  Game = game:create(1, 2, cells([
    {5,5,white,queen},
    {3,3,white},
    {4,4,black}
  ], white)),
  ?assertMatch({error, rules_jump_over_own}, game:move(Game, ?WHITE_QUEEN, {2,2})).

white_checker_goes_to_end_line_became_queen_test() ->
  Game = game:create(1,2, cells([
    {2,2,white},
    {5,5,black}
  ], white)),
  {Res, Game2} = ?MOVE(Game, {2,2}, [u,r]),
  ?assertEqual(ok, Res),
  ?assertEqual(#game_state{checkers = [
    #checker{x=3,y=1,side=white,type=queen},
    #checker{x=5,y=5,side=black,type=simple}
  ], whose_turn = black}, game:get_state(Game2)).

black_checker_goes_to_end_line_became_queen_test() ->
  Game = game:create(1,2, cells([
    {2,2,white},
    {7,7,black}
  ], black)),
  {Res, Game2} = ?MOVE(Game, {7,7}, [d,r]),
  ?assertEqual(ok, Res),
  ?assertEqual(#game_state{checkers = [
    #checker{x=8,y=8,side=black,type=queen},
    #checker{x=2,y=2,side=white,type=simple}
  ], whose_turn = white}, game:get_state(Game2)).