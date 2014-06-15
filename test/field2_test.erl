
-module(field2_test).
-author("Alex").

-include_lib("eunit/include/eunit.hrl").

field_create_test() ->
  _F = field2:create(2, 2),
  _F1 = field2:create(1, 1).

field_is_empty_test() ->
  F = field2:create(1,1),
  ?assertEqual(true, field2:is_empty(F, 1, 1)).

field_put_get_test() ->
  F = field2:create(1,1),
  F1 = field2:put(F, 1, 1, {item, 4}),
  ?assertEqual({item, 4}, field2:get(F1, 1, 1)).

field_is_empty_after_put_test() ->
  F = field2:create(1,1),
  F1 = field2:put(F, 1, 1, {item, 4}),
  ?assertEqual(false, field2:is_empty(F1, 1, 1)).

field_put_replace_test() ->
  F = field2:create(1,1),
  F1 = field2:put(F, 1, 1, {item, 4}),
  F2 = field2:put(F1, 1, 1, {replacement, 5}),
  ?assertEqual({replacement, 5}, field2:get(F2, 1, 1)).

field_move_test() ->
  F = field2:create(1,1),
  F1 = field2:put(F, 1, 1, {item, 4}),
  F2 = field2:move(F1, 1, 1, 2, 2),
  ?assertEqual({item, 4}, field2:get(F2, 2, 2)),
  ?assertEqual(true, field2:is_empty(F2, 1, 1)).

field_count_type_test() ->
  F = field2:create(2,2),
  ?assertEqual(0, field2:count(F, item)),
  F1 = field2:put(F, 1, 2, {item, 5}),
  ?assertEqual(1, field2:count(F1, item)).

field_count_fn_test() ->
  F = field2:create(2,2),
  Only5 = fun ({_, Nr}) -> Nr == 5 end,
  ?assertEqual(0, field2:count(F, Only5)),
  F1 = field2:put(F, 1, 2, {item, 4}),
  ?assertEqual(0, field2:count(F1, Only5)),
  F2 = field2:put(F1,2, 1, {some, 5}),
  ?assertEqual(1, field2:count(F2, Only5)).


field_delete_test() ->
  F = field2:create(1,1),
  F1 = field2:put(F, 1, 1, {item, 4}),
  F2 = field2:delete(F1, 1, 1),
  ?assertEqual({empty}, field2:get(F2, 1, 1)).

field_is_test() ->
  F = field2:create(1,1),
  ?assertEqual(true, field2:is(F, 1, 1, empty)),
  F1 = field2:put(F, 1, 1, {something, else}),
  ?assertEqual(true, field2:is(F1, 1, 1, something)).

field_cell_type_test() ->
  F = field2:create(1,1),
  ?assertEqual(empty, field2:cell_type(F, 1, 1)),
  F1 = field2:put(F, 1, 1, {something, else}),
  ?assertEqual(something, field2:cell_type(F1, 1, 1)).

field_multu_put_test() ->
  F = field2:create(2,2),
  F1 = field2:put(F, [{1,1,{a}}, {2,2,{b}}]),
  ?assertEqual({a}, field2:get(F1, 1, 1)),
  ?assertEqual({b}, field2:get(F1, 2, 2)).
