%%%-------------------------------------------------------------------
%%% core_sup监控树的子进程（游戏的服务进程）规范配置。
%%%-------------------------------------------------------------------
-module(services).

%% API
-export([cfg/0, get/1]).

-include("service.hrl").

cfg() ->
    {ok, [acceptor_sup, sys_listener]}.

get(acceptor_sup = Id) ->
    {ok, #service{
        id = Id,
        name = "acceptor监督树",
        type = supervisor,
        mfa  = {Id, start_link, []}
    }};

get(sys_listener = Id) ->
    {ok, #service{
        id = Id,
        name = "socket监听服务",
        mfa = {Id, start_link, []}
    }};

get(_Name) ->
    {error, undefined_service}.