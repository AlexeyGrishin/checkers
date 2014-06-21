-module(field2).
-author("Alex").

%% API
-export([create/2, delete/3, list/1, put/4, put/2, move/5, is_empty/3, get/3, dump/2, is/4, cell_type/3, count/2]).

-include("field.hrl").

%public

create(Width, Height) ->
  #field{width = Width, height = Height, cells = []}.

get(#field{} = F, X, Y) ->
  Cells = F#field.cells,
  find_by_coords(Cells, X, Y).

list(#field{cells = Cells}) ->
  lists:map(fun ({cell, X, Y, Item}) -> {X, Y, Item} end, Cells).     %TODO:ugly

delete(#field{} = F, X, Y) ->
  F#field{cells = delete_by_coords(F#field.cells, X, Y)}.

put(#field{} = F, X, Y, Item) ->
  F#field{cells = put_by_coords(F#field.cells, X, Y, Item)}.

put(#field{} = F, [{X, Y, Item} | Tail]) ->  %TODO: optimize
  put(
    ?MODULE:put(F, Tail),
    X, Y, Item);
put(#field{} = F, []) -> F.

move(#field{} = F, Xfrom, Yfrom, Xto, Yto) -> %TODO: optimize
  case find_by_coords(F#field.cells, Xfrom, Yfrom) of
    {empty} -> F;
    Item ->
      F#field{cells = put_by_coords(
        delete_by_coords(F#field.cells, Xfrom, Yfrom), Xto, Yto, Item
      )}
  end.

count(#field{cells = Cells}, Fn) when is_function(Fn) ->
  length(lists:filter(fun(#cell{item = Item}) -> Fn(Item) end, Cells));
count(#field{} = F, Type) when is_atom(Type) ->
  count(F,
    fun(Item) -> element(1, Item) == Type end
  ).

is_empty(#field{} = F, X, Y) ->
  {empty} =:= find_by_coords(F#field.cells, X, Y).

cell_type(#field{} = F, X, Y) ->
  element(1, find_by_coords(F#field.cells, X, Y)).

is(#field{} = F, X, Y, Type) ->
  Type == cell_type(F, X, Y).

dump_line(X, Getter, Width, ToCharFn) when X =< Width ->
  [ToCharFn(Getter(X)) | dump_line(X + 1, Getter, Width, ToCharFn)];
dump_line(X, _Getter, Width, _ToCharFn) when X > Width ->
  [].


dump(#field{width = W, height = H, cells = Cells}, ToCharFn) ->
  %TODO: draw a field instead.
  RowNrs = lists:seq(1, H),
  Rows = lists:map(fun (LineNr) ->
    string:join(
      dump_line(1, fun (X) -> find_by_coords(Cells, X, LineNr) end, W, ToCharFn),
    "")

  end, RowNrs),
  Rows.
  %string:join(Rows, [10,13]).

%private

by_coords(X, Y) ->
  fun
    (#cell{x = X1, y = Y1}) ->
      (X1 == X) and (Y == Y1);
    (_) -> false
  end.

not_by_coords(X, Y) ->
  Fun = by_coords(X, Y),
  fun (Item) -> not Fun(Item) end.

find_by_coords(List, X, Y) ->
  case lists:filter(by_coords(X, Y), List) of
    [#cell{item = Item} | Tail] when length(Tail) == 0 -> Item;
    [] -> {empty};
    Other -> "broken list - there are several items on same coord: " + string:join(Other, ",")
  end.

delete_by_coords(List, X, Y) ->
  lists:filter(not_by_coords(X, Y), List).

put_by_coords(List, X, Y, Item) ->
  [#cell{x = X, y = Y, item = Item} | delete_by_coords(List, X, Y)].






