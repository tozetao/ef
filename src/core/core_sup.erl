%%%-------------------------------------------------------------------
%%% @author zetao
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 9月 2020 11:55
%%%-------------------------------------------------------------------
-module(core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).



%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = #{
        strategy  => one_for_one,
        intensity => MaxRestarts,
        period    => MaxSecondsBetweenRestarts
    },

    AChild = #{
        id    => 'sys_boot',
        start => {'sys_boot', start_link, []},
        restart  => transient,
        shutdown => 10 * 1000,
        type     => worker,
        modules  => ['sys_boot']
    },

    {ok, {SupFlags, [AChild]}}.