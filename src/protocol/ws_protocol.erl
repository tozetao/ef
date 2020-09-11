%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 处理websocket协议的模块
%%% @end
%%% Created : 10. 9月 2020 14:45
%%%-------------------------------------------------------------------
-module(ws_protocol).
-author("Administrator").
-include("common.hrl").

%% API
-export([parse/1]).

%% 解析
parse(Socket) ->
    case handshake(Socket) of
        ok ->
            create_conn(Socket);
        _ ->
            fail
    end.

%% 私有函数

%% 握手校验
handshake(Socket) ->
    receive
        {tcp, Socket, HeaderData} ->
            HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
            HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList],
            case lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1) of
                false ->
                    gen_tcp:close(Socket),
                    fail;
                {_, SecWebSocketKey} ->
                    Sha1 = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
                    Base64 = base64:encode(Sha1),
                    Handshake = [
                        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                        <<"Upgrade: websocket\r\n">>,
                        <<"Connection: Upgrade\r\n">>,
                        <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                        <<"\r\n">>
                    ],
                    gen_tcp:send(Socket, Handshake),
                    ok
            end;
        _Else ->
            gen_tcp:close(Socket),
            fail
    end.

%% 当握手成功后，创建一个连接进程来处理socket
create_conn(Socket) ->
    try
        {ok, Pid} = ws_conn:start(Socket),
        gen_tcp:controlling_process(Socket, Pid)
    catch
        E: Ex ->
            ?ERR("create sys_conn fail, ~w: ~w", [E, Ex]),
            gen_tcp:close(Socket)
    end.


%% 编码

%% 解码