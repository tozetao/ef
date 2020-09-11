%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% sys_acceptor进程会侦听Listen Socket，处理客户端连接
%%% @end
%%%-------------------------------------------------------------------
-module(sys_acceptor).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include("common.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

loop() ->
    gen_server:cast(?MODULE, loop).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ListenSocket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ListenSocket], []).



init([ListenSocket]) ->
    loop(),
    {ok, [ListenSocket]}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(loop, State = [ListenSocket]) ->
    %% 侦听ListenSocket，处理客户端连接。

    %% 这里的closed是连接进程关闭了么？
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, once}]),
            gen_tcp:controlling_process(Socket, spawn(fun() -> accept(Socket) end));
        {error, closed} ->
            ?INFO("socket closed"),
            ignore;
        {error, SocketAcceptFail} ->
            ?ERR("socket accept fail, reason: ~w", [SocketAcceptFail])
    end,
    loop(),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 处理socket连接，在这里选择协议模块来处理socket数据，未实现
accept(Socket) ->
    ws_protocol:parse(Socket),
    ?INFO("handle process closed.").