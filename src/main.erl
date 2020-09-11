%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(main).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case core_sup:start_link() of
        {ok, Pid}  ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
