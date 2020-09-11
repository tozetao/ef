%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 网络监听进程。
%%% 它会监听网络端口，并开启多个acceptor去侦听listener socket。
%%% @end
%%%-------------------------------------------------------------------
-module(sys_listener).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include("common.hrl").

-define(SERVER, ?MODULE).
-define(SOCKET_OPTIONS, [
    binary,
    {packet, 0},
    {active, false},
    {reuseaddr, true},
    {nodelay, false},
    {delay_send, true},
    {send_timeout, 100000},
    {send_timeout_close, false},
    {exit_on_close, false}
]).

%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ?INFO("sys_listener service starting..."),

    erlang:process_flag(trap_exit, true),

    listen().

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?ERR("sys_listener terminate"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
listen() ->
    Port = 3000,

    case gen_tcp:listen(Port, ?SOCKET_OPTIONS) of
        {ok, ListenSocket} ->
            create_acceptor(1, ListenSocket),
            {ok, []};
        {error, Reason} ->
            ?ERR("sockent listen error, reason: ~w", [Reason]),
            {stop, socket_listen_fail}
    end.

%% 如果accept成功，那么表示连接建立
%% 由多个sys_acceptor进程来侦听listen socket
create_acceptor(0, _) ->
    ok;
create_acceptor(N, ListenSocket) ->
    case supervisor:start_child(acceptor_sup, [ListenSocket]) of
        {error, Reason} ->
            ?ERR("start sys_acceptor error, reason: ~w", [Reason]);
        _ ->
            ok
    end,
    create_acceptor(N - 1, ListenSocket).