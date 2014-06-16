-module(game).
-author("Alex").

%% API
-export([
  create/2, create/4,
  dump/1, describe/1,
  whose_turn/1, player/2, players/1, winner/1,
  move/3
]).

-record(game, {field, white_player, black_player, turn, winner = false}).
-include("field.hrl").

%public

create(White, Black) ->
  #game{field = create_field(), white_player = White, black_player = Black, turn = white}.

create(White, Black, Cells, WhosTurn) ->
  check_win(#game{field = create_field(Cells), white_player = White, black_player = Black, turn = WhosTurn}).

dump(#game{field = Field}) ->
  field2:dump(Field, fun (Item) ->
    case Item of
      {empty} -> ".";
      {checker, white, _} -> "o";
      {checker, black, _} -> "*"
    end
  end).

player(#game{white_player = Player}, white) -> {white, Player};
player(#game{black_player = Player}, black) -> {black, Player}.

players(#game{white_player = WP, black_player = BP}) -> [{white, WP}, {black, BP}].

whose_turn(#game{turn = Turn} = Game) -> player(Game, Turn).

winner(#game{winner = false}) -> false;
winner(#game{winner = W} = Game) -> player(Game, W).


move(#game{field = Field, turn = Side, winner = false} = Game, {X0, Y0}, {X1, Y1}) when ?IS_INSIDE(X1, Y1, Field) ->
  Opposite = opposite(Side),
  ItemFrom = what_on_cell(Game, X0, Y0),
  ItemTo = what_on_cell(Game, X1, Y1),
  {Direction, Distance, MiddlePoint} = direction(X0, Y0, X1, Y1, Side),
  ItemMiddle = case MiddlePoint of
    {X, Y} -> what_on_cell(Game, X, Y);
    _ -> false
  end,

  case {ItemFrom, ItemTo, Direction, Distance} of
    {Side, empty, forward, D} when D =< 2 ->
      AfterMove = field2:move(Field, X0, Y0, X1, Y1),
      case ItemMiddle of
        false -> {ok, Game#game{field = AfterMove, turn = Opposite}};
        empty -> {error, rules_jump};
        Opposite ->
          {MX, MY} = MiddlePoint,
          AfterEat = field2:delete(AfterMove, MX, MY),
          {ok, check_win(Game#game{field = AfterEat, turn = Opposite})}
        ;
        Side -> {error, rules_jump_over_self}
      end;
    {Opposite, _, _, _}           -> {error, rules_opposite_from};
    {empty, _, _, _}              -> {error, rules_empty_from};
    {Side, empty, backward, _}    -> {error, rules_backward};
    {_, _, unknown, _}            -> {error, rules_direction};
    {Side, _, _, _}               -> {error, rules_empty_to};
    {_, _, _, D} when D >= 2      -> {error, rules_jump};
    _                             -> {error, state_unknown}
  end;
move(#game{field = Field, winner = false}, _, {X1, Y1}) when not ?IS_INSIDE(X1, Y1, Field) ->
  {error, rules_outside};
move(#game{winner = _W}, _P1, _P2) ->
  {error, state_gameover}.

describe({error, rules_empty_to})       -> "Target cell is not empty";
describe({error, rules_empty_from})     -> "Source cell is empty - nothing to move";
describe({error, rules_opposite_from})  -> "Source cell belongs to opposite side";
describe({error, state_unknown})        -> "Unknown error";
describe({error, rules_jump})           -> "Cannot jump over several cells";
describe({error, rules_jump_over_self}) -> "Cannot jump over own checkers";
describe({error, rules_direction})      -> "Cannot move that way";
describe({error, rules_outside})        -> "Cannot move outside field";
describe({error, SomethingElse})        -> "Error: " + atom_to_list(SomethingElse).

%private
only(Side) ->
  fun
    ({checker, CSide, _}) -> Side == CSide;
    (_) -> false
  end.


check_win(#game{field = Field, winner = false} = Game) ->
  BlackCount = field2:count(Field, only(black)),
  WhiteCount = field2:count(Field, only(white)),
  case {WhiteCount, BlackCount} of
    {0, 0} -> {error, state_field_is_empty};
    {0, _} -> Game#game{winner = black};
    {_, 0} -> Game#game{winner = white};
    _ -> Game
  end.

opposite(white) -> black;
opposite(black) -> white.

delta(C0, C1) -> C1 - C0.
dir(C0, C1) ->
  case C1 - C0 of
    N when N < 0 -> -1;
    N when N > 0 -> 1;
    _ -> 0
  end.

step(X0, Y0, _X1, _Y1, 0) -> {X0, Y0};
step(X0, Y0, X1, Y1, Step) ->
  Dx = dir(X0, X1),
  Dy = dir(Y0, Y1),
  step(X0 + Dx, Y0 + Dy, X1, Y1, Step-1).



is_diagonal(X0, Y0, X1, Y1) -> abs(delta(X0, X1)) == abs(delta(Y0, Y1)).

direction_of(Y0, Y1, white) ->
  case dir(Y0, Y1) of
    1 -> backward;
    -1 -> forward;
    _ -> unknown
  end;
direction_of(Y0, Y1, black) ->
  case dir(Y0, Y1) of
    1 -> forward;
    -1 -> backward;
    _ -> unknown
  end.

direction(X0, Y0, X1, Y1, Side) ->
  case {is_diagonal(X0, Y0, X1, Y1), abs(delta(Y0, Y1)), direction_of(Y0, Y1, Side)} of
    {true, 1, Direction} -> {Direction, 1, false};
    {true, Distance, Direction} -> {Direction, Distance, step(X0, Y0, X1, Y1, 1)};
    {false, _, _} -> {unknown, 0, false}
  end.


what_on_cell(#game{field = F}, X, Y) ->
  case field2:get(F, X, Y) of
    {empty} -> empty;
    {checker, Side, _} -> Side
  end.


% field initiators
create_field(Cells) ->
  Field = field2:create(8, 8),
  field2:put(Field, lists:map(
    fun ({X, Y, Side}) -> {X, Y, create_checker(Side)} end,
    Cells)
  )
  .
create_field() ->
  fill_created_field(field2:create(8, 8)).

fill_created_field(F) ->
  fill_lines(F, [
    {1, black, 1},
    {2, black, 2},
    {3, black, 1},
    {6, white, 2},
    {7, white, 1},
    {8, white, 2}
  ])
  .

create_checker(Side) -> {checker, Side, simple}.

fill_lines(F, [{LineNr, Side, From} | Tail]) ->
  fill_lines(
    fill_line(F, LineNr, Side, From),
    Tail
  );
fill_lines(F, []) -> F.

fill_line(F, LineNr, Side, From) ->
  field2:put(F,
    lists:map(
      fun (N) -> {From+N, LineNr, create_checker(Side)} end,
      [0,2,4,6]
    )
  )
.

