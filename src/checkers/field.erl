%%%-------------------------------------------------------------------
%%% @author Alex
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2014 2:33 AM
%%%-------------------------------------------------------------------
-module(field).
-author("Alex").
%% API
-export([replace/3]).
-export([create/2, delete/3, put/4, is_empty/3, get/3, dump/2, is/4, cell_type/3]).


replace(List, Pos, _) when length(List) < Pos
  -> {error, "cannot replace"};
replace([_|Tail], 1, Replacement)
  -> [Replacement | Tail];
replace([Head|Tail], Pos, Replacement)
  -> [Head | replace(Tail, Pos-1, Replacement)].


create(Width, Height)
  when is_number(Width) and is_number(Height)
  -> {field, Width, Height, lists:map(fun (_) ->
      lists:map(fun (_) -> {empty} end, lists:seq(1, Width))
     end , lists:seq(1, Height))}.

put({field, W, H, Field}, X, Y, Item) ->
  {field, W, H, replace(Field, Y,
    replace(lists:nth(Y, Field), X, Item)
  )}.

delete(F, X, Y) -> put(F, X, Y, {empty}).

get(F, X, Y) ->
  {field, _, _, Field} = F,
  lists:nth(X, lists:nth(Y, Field)).

is_empty(F, X, Y) ->
  case get(F, X, Y) of
    {empty} -> true;
    _ -> false
  end.

is(F, X, Y, Tag) ->
  case cell_type(F, X, Y) of
    Tag -> true;
    _ -> false
  end.

cell_type(F, X, Y) -> element(1, get(F,X,Y)).

dump({field, _, _, Field}, ToCharFn) ->

  string:join(lists:map(fun (Line) ->
    string:join(lists:map(ToCharFn, Line), "")
  end, Field), [10,13]).
