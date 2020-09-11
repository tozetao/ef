%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 启动应用中所有的服务进程
%%% @end
%%%-------------------------------------------------------------------
-module(sys_boot).

-include("common.hrl").
-include("service.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

init_services() ->
    gen_server:cast(?SERVER, init_services).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    ?INFO("sys_boot service starting..."),
    init_services(),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(init_services, State) ->
    {ok, List} = services:cfg(),
    start_services(List),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("sys_boot terminate\n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 启动应用的服务进程，服务进程包括系统进程、游戏功能进程，服务进程都是动态添加到core_sup监督树中的。
%% 系统进程主要是处理网络连接的进程，acceptor是一个子督程，listener是它的子进程。
start_services([]) ->
    ?INFO("all services completed."),
    ok;
start_services([Element | Tail]) ->
    case services:get(Element) of
        {ok, #service{id = Id, mfa = MFA, restart = Restart, type = Type}} ->
            ChildSpec = #{
                id => Id,
                start => MFA,
                restart => Restart,
                shutdown => 20000,
                type => Type,
                modules => [Id]
            },
            case supervisor:start_child(core_sup, ChildSpec) of
                {ok, _Pid} ->
                    start_services(Tail);
                {error, Reason} ->
                    ?ERR("start ~p service error, ~w~n", [Id, Reason]),
                    {error, Reason}
            end ;
        {error, Reason} ->
            ?ERR("the ~p configuration was not found.", [Element]),
            {error, Reason}
    end.