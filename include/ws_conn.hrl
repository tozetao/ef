%% 连续帧
-define(CONTINUATION_FRAME, 0).

%% 文本帧
-define(TEXT_FRAME, 1).

%% 二进制帧
-define(BIN_FRAME, 2).

-record(ws_frame, {
    opcode = 0,
    length = 0,     %% 包体长度，单位：字节
    extend_len = 0  %% payload扩展长度，单位：bit位
}).

-record(conn, {
    socket,
    ip = "",
    status = unread_head,
    ws_frame = #ws_frame{}
}).