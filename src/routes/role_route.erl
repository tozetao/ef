%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 9月 2020 18:08
%%%-------------------------------------------------------------------
-module(role_route).
-author("Administrator").

%% API
-export([match/1]).


match(Cmd) ->
    {role, add};

match(_) ->
    error.