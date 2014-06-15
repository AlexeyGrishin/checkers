
-module(field_test).
-author("Alex").

-include_lib("eunit/include/eunit.hrl").

replace_one_element_test() -> ?assertEqual([x], field:replace([2],1,x)).
replace_first_test() -> ?assertEqual([x,10], field:replace([9,10],1,x)).
replace_last_test() -> ?assertEqual([9,x], field:replace([9,10],2,x)).
replace_error2_test() -> ?assertEqual({error, "cannot replace"}, field:replace([1], 2, y)).

field_create_test() ->
  _F = field:create(2, 2),
  _F1 = field:create(1, 1).

field_is_empty_test() ->
  F = field:create(1,1),
  ?assertEqual(true, field:is_empty(F, 1, 1)).

field_put_get_test() ->
  F = field:create(1,1),
  F1 = field:put(F, 1, 1, {item, 4}),
  ?assertEqual({item, 4}, field:get(F1, 1, 1)).

field_is_empty_after_put_test() ->
  F = field:create(1,1),
  F1 = field:put(F, 1, 1, {item, 4}),
  ?assertEqual(false, field:is_empty(F1, 1, 1)).

field_put_replace_test() ->
  F = field:create(1,1),
  F1 = field:put(F, 1, 1, {item, 4}),
  F2 = field:put(F1, 1, 1, {replacement, 5}),
  ?assertEqual({replacement, 5}, field:get(F2, 1, 1)).

field_delete_test() ->
  F = field:create(1,1),
  F1 = field:put(F, 1, 1, {item, 4}),
  F2 = field:delete(F1, 1, 1),
  ?assertEqual({empty}, field:get(F2, 1, 1)).

field_is_test() ->
  F = field:create(1,1),
  ?assertEqual(true, field:is(F, 1, 1, empty)),
  F1 = field:put(F, 1, 1, {something, else}),
  ?assertEqual(true, field:is(F1, 1, 1, something)).

field_cell_type_test() ->
  F = field:create(1,1),
  ?assertEqual(empty, field:cell_type(F, 1, 1)),
  F1 = field:put(F, 1, 1, {something, else}),
  ?assertEqual(something, field:cell_type(F1, 1, 1)).
