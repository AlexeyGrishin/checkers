%%%-------------------------------------------------------------------
%%% @author Alex
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2014 2:54 AM
%%%-------------------------------------------------------------------
-module(fstat).
-author("Alex").
%% API
-export([deep_element/2]).
-export([file_size/1]).
-export([to_bytes/1, folders_larger_than/2]).

deep_element([], Any) -> Any;
deep_element([Head | Tail], Tuple) when is_tuple(Tuple) and is_number(Head) ->
  deep_element(element(Head, Tuple), Tail) .

file_type(Path) -> case file:read_file_info(Path) of
                     {ok, {file_info, Size, regular, _, _, _, _, _, _, _, _, _, _, _}} -> {file, Path, Size};
                     {ok, {file_info, 0, directory, _, _, _, _, _, _, _, _, _, _, _}} -> {directory, Path, unknown};
                     {error, Error} -> {error, Error}
                   end.

file_size(Path) -> case file_type(Path) of
                     {file, _, Size} -> Size;
                     {directory, _, _} ->
                       {ok, Files} = file:list_dir(Path),
                       ChildSize = fun (Name) -> file_size(string:concat(Path, string:concat("/", Name))) end,
                       lists:sum(lists:map(ChildSize, Files))
                   end.


to_bytes({N, mb}) -> N*1024*1024;
to_bytes({N, kb}) -> N*1024;
to_bytes({N, bytes}) -> N;
to_bytes(N) when is_number(N) -> N.


from_bytes(N) when N < 1024 -> {N, bytes};
from_bytes(N) when N < 1024*1024 -> {N/1024, kb};
from_bytes(N) -> {N/1024/1024, mb}.

folders_larger_than(Path, Size) when is_tuple(Size) -> folders_larger_than(Path, to_bytes(Size));
folders_larger_than(Path, Bytes) when is_number(Bytes) ->
  case file:list_dir(Path) of
    {ok, Files} ->
      ChildSizes = fun (Name, List) ->
        FullName = string:concat(Path, string:concat("/", Name)),
        case file_type(FullName) of
          {directory, _, _} -> List ++ folders_larger_than(FullName, Bytes);
          {file, _, Size} -> [{file, FullName, Size} | List];
          {error, _} -> List
        end
      end,
      ToSize = fun ({_, _, Size}) -> to_bytes(Size) end,
      FoldersOnly = fun
        ({directory, _, Size}) -> to_bytes(Size) > Bytes;
        (_) -> false
      end ,

      Sizes = lists:foldl(ChildSizes, [], Files),
      MySize = lists:sum(lists:map(ToSize, Sizes)),
      %io:write(lists:filter(FoldersOnly, Sizes)),
      case lists:filter(FoldersOnly, Sizes) of
        [] -> [{directory, Path, from_bytes(MySize)}];
        List -> List
      end;
    {error, _} -> [] %ignore
  end.


