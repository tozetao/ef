%%%-------------------------------------------------------------------
%%% @author zetao
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 9月 2020 18:26
%%%-------------------------------------------------------------------
-module(acceptor_sup).
-behaviour(supervisor).
-export([init/1]).

-include("common.hrl").

%% API
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    ?INFO("acceptor_sup service starting..."),

    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => 10,
        period    => 1
    },

    AChild = #{
        id    => 'sys_acceptor',
        start => {'sys_acceptor', start_link, []},
        restart  => transient,
        shutdown => 2000,
        type     => worker,
        modules  => ['sys_acceptor']
    },

    {ok, {SupFlags, [AChild]}}.