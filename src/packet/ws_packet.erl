%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 9月 2020 16:58
%%%-------------------------------------------------------------------
-module(ws_packet).
-author("Administrator").

%% API
-export([unpacket/2, packet/1, packet/2]).

%% 解析数据，Len是Payload的长度
unpacket(Bin, Len) ->
    <<Masking:4/binary, Payload:Len/binary>> = Bin,
    unpacket(Payload, Masking, <<>>).

unpacket(Payload, <<MA:8, MB:8, MC:8, MD:8>> = Masking, Acc) ->
    case size(Payload) of
        0 ->
            Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unpacket(Rest, Masking, Acc1)
    end.


%% 打包符合websocket数据帧的格式
packet(Bin) ->
    packet(Bin, []).

packet(Bin, Opts) ->
    %% 默认文本类型
    Opcode =
        case lists:keyfind(opcode, 1, Opts) of
            {opcode, V} ->
                V;
            _ ->
                1
        end,

    DataSize = erlang:byte_size(Bin),
    {Len, ExLen} =
        case DataSize of
            D when D < 126 ->
                {DataSize, 0};
            D when D < 65536 ->
                {126, D};
            _ ->
                {127, DataSize}
        end,
    Frame =
        case ExLen of
            0 ->
                <<1:1, 0:3, Opcode:4, 0:1, Len:7, Bin/binary>>;
            E when E > 65535 ->
                <<1:1, 0:3, Opcode:4, 0:1, Len:7, ExLen:64, Bin/binary>>;
            _ ->
                <<1:1, 0:3, Opcode:4, 0:1, Len:7, ExLen:16, Bin/binary>>
        end,
    Frame.