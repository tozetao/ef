%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ws_conn).

-behaviour(gen_server).

-export([start/1, reply/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include("common.hrl").
-include("ws_conn.hrl").

-define(SERVER, ?MODULE).
-define(MAX_LEN, 524288). %% 限制最大的单条协议长度


%%%===================================================================
%%% API
%%%===================================================================

reply(SocketId, Data) ->
    SocketId ! {ws_reply, ws_packet:packet(Data)}.


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

init([Socket]) ->
    process_flag(trap_exit, true),

    {ok, {Ip, _Port}} = inet:peername(Socket),
    State = #conn{
        socket = Socket,
        ip = Ip
    },

    %% 开始接收数据帧
    recv(),

    ?INFO("连接建立成功...."),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(recv, State) ->
    recv_frame_head(State);



%%%===================================================================
%%% web socket 包头解析
%%%===================================================================

%% 包头长度判断
handle_info({inet_async, _Socket, _Ref, {ok, <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7>>}},
    State = #conn{status = read_head}) when Len > ?MAX_LEN ->
    ?ERR("发送的socket数据过长: ~w", [Len]),
    {stop, normal, State};

%% 解析包头
handle_info({inet_async, Socket, _Ref, {ok, _Bin = <<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7>>}},
    State = #conn{status = read_head}) ->
     ?INFO("包头长度:~w, Opcode: ~w", [Len, Opcode]),
    case Opcode of
        0 ->
            %% 连续帧会导致进程无法再次读取数据？？？？
            {noreply, State#conn{status = parse_len}};
        _ ->
            parse_payload_len(Len, Opcode, Socket, State)
    end;


%%%===================================================================
%%% web socket payload 长度解析
%%%===================================================================

handle_info({inet_async, Socket, _Ref, {ok, <<DataLen:16>>}},
    State = #conn{status = parse_len, ws_frame = #ws_frame{extend_len = 16}}) ->

    prim_inet:async_recv(Socket, DataLen, 60000),
    NewState = State#conn{status = completed, ws_frame = #ws_frame{length = DataLen}},
    {noreply, NewState};

handle_info({inet_async, Socket, _Ref, {ok, <<DataLen:64>>}},
    State = #conn{status = parse_len, ws_frame = #ws_frame{extend_len = 64}}) ->

    prim_inet:async_recv(Socket, DataLen, 60000),
    NewState = State#conn{status = completed, ws_frame = #ws_frame{length = DataLen}},
    {noreply, NewState};


%%%===================================================================
%%% web socket 包体数据处理
%%%===================================================================

handle_info({inet_async, _Socket, _Ref, {ok, Bin = <<_MaskKey:4/binary, _Rest/binary>>}},
    State = #conn{status = completed, ws_frame = #ws_frame{length = Length, opcode = Opcode}}) ->

    %% 对数据帧进行解码，得到包体数据
    Data = ws_packet:unpacket(Bin, Length),

    ?ERR("opcode: ~p, data: ~p", [Opcode, Data]),

    %% 处理包体数据后，status状态需要改变，ws_frame要重置
    case Opcode of
        %% 目前只处理文本帧
        ?TEXT_FRAME ->
            ws_conn:reply(self(), <<"server reply">>),
            {noreply, State};
        ?BIN_FRAME ->
            {stop, normal, State};
        _ ->
            {stop, normal, State}
    end;


%%%===================================================================
%%% Socket 回复消息处理
%%%===================================================================
handle_info({ws_reply, Bin}, State = #conn{socket = Socket}) ->
    case catch erlang:port_command(Socket, Bin, [nosuspend]) of
        true ->
            {noreply, State};
        false ->
            {noreply, State};
        Else ->
            ?ERR("发送socket数据失败: ~p", [Else]),
            {noreply, State}
    end;


%%%===================================================================
%%% socket异常处理
%%%===================================================================
%% 客户端连接断开
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State) ->
    ?ERR("inet_async closed"),
    {stop, normal, State};


%%%===================================================================
%%% socket异常处理
%%%===================================================================

%% 收到异常数据
handle_info({inet_async, _Socket, _Ref, {ok, Bin}}, State = #conn{ip = Ip}) ->
    ?ERR("IP:~w发送了无效请求: ~w", [Ip, Bin]),
    {noreply, State};

%% 接收socket数据时发生了未预料的错误
handle_info({inet_async, _Socket, _Ref, {error, Reason}}, State) ->
    ?ERR("接收数据发生了未知错误：~w", [Reason]),
    {stop, normal, State};

%%%===================================================================
%%% 关联进程的异常处理
%%%===================================================================
handle_info({'EXIT', Pid, Why}, State) ->
    ?ERR("Pid: ~p, Why: ~p", [Pid, Why]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?INFO("handle_info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?INFO("ws_conn terminate, ~p.", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
recv() ->
    self() ! recv.

%% 处理数据包的状态：包头未读、包头读取中、解析数据长度，解析完毕，unread_head、read_head、
recv_frame_head(State = #conn{socket = Socket, status = unread_head}) ->
    ?ERR("start receive frame head"),
    R = prim_inet:async_recv(Socket, 2, 60000),
    ?ERR("async recv: ~p\n", [R]),
    {noreply, State#conn{status = read_head}};
recv_frame_head(State) ->
    {noreply, State}.

parse_payload_len(PayloadLen, Opcode, Socket, State) ->
    ?INFO("parse_payload_len"),

    case PayloadLen of
        %% 后面的俩个字节是包体长度，因此再读取俩个字节
        126 ->
            prim_inet:async_recv(Socket, 2, 60000),
            ExtendLen = 2 * 8,
            {noreply, State#conn{status = parse_len, ws_frame = #ws_frame{extend_len = ExtendLen, opcode = Opcode}}};
        %% 后面的8个字节是包体长度
        127 ->
            %%                    len = 8 * 8, open_code = Opcode
            prim_inet:async_recv(Socket, 8, 60000),
            ExtendLen = 8 * 8,
            {noreply, State#conn{status = parse_len, ws_frame = #ws_frame{extend_len = ExtendLen, opcode = Opcode}}};
        %% 正常数据长度
        _ ->
            prim_inet:async_recv(Socket, PayloadLen + 4, 60000),
            %% length = Len, open_code = Opcode
            {noreply, State#conn{status = completed, ws_frame = #ws_frame{length = PayloadLen, opcode = Opcode}}}
    end.

%% 验证payload的长度
validate_payload_len() ->
    ok.