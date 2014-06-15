%%%-------------------------------------------------------------------
%%% @author Alex
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2014 2:01 PM
%%%-------------------------------------------------------------------
-module(fstat_test).
-author("Alex").

-include_lib("eunit/include/eunit.hrl").


to_bytes_mb_test() ->
  ?assert(fstat:to_bytes({1, mb}) =:= 1024*1024).
