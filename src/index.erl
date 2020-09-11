%%%-------------------------------------------------------------------
%%% @author zetao
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 入口文件
%%% @end
%%% Created : 09. 9月 2020 16:29
%%%-------------------------------------------------------------------
-module(index).

-define(APPS, [crypto, main]).

%% API
-export([start/0, stop/0]).

start() ->
    start_applications().

stop() ->
    stop_applications().


%%%===================================================================
%%% Internal functions
%%%===================================================================

start_applications() ->
    %% 遍历所有应用并启动，如果应用已启动则忽略，如果出错则需要关闭之前启动的应用。
    executor(
        fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        ?APPS
    ).


stop_applications() ->
    %% 遍历所有应用并关闭，如因应用没有启动则忽略，如果关闭失败则需要重新启动之前关闭的应用。
    executor(
        fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        ?APPS
    ).

executor(Iterator, Do, Undo, SkipError, Apps) ->
    F = fun (App, Acc) ->
        case Do(App) of
            ok ->
                [App | Acc];
            {error, {SkipError, _}} ->
                Acc;
            {error, Reason} ->
                lists:foreach(Undo, Acc),
                throw({error, Reason})
        end
        end,

    Iterator(F, [], Apps),
    ok.