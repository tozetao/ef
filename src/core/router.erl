%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 9月 2020 17:36
%%%-------------------------------------------------------------------
-module(router).
-author("Administrator").

%% API
-export([]).


handle(Bin) ->
    %% 使用双方协商的协议解码

    %% 根据路由规则
    Cmd = 1101,

    case dispatch(Cmd) of
        not_found ->
            false;
        Route ->
            %% 找到要分执行的模块和函数
            {Module, Func} = Route:match(Cmd),

            Module:Func(Bin)
    end.



dispatch(Cmd) ->
    case Cmd div 100 of
        11 ->
            role_route;
        12 ->
            room_route;
        _ ->
            not_found
    end.

%% 1101 % 100

%%	处理结果有：
%%	不响应
%%	更新连接进程的状态
%%	响应并更新连接进程状态
%%	失败响应错误信息
%%	其他失败：shell报错
%%
%%  1000 % 10 =

%% 我的路由规则要如何设计？
%% 如果开启一个路由进程来处理，
%%
%% 以前框架的设计是取模，再使用模块的函数匹配来实现。
%% 我能不能按照以前的方式，id对应模块名和函数，然后来处理请求呢？
%%
%% 我不想开一个路由进程，感觉这样会让该进程称为系统瓶颈。
%% 还是像之前一样吧，分模块来处理。
%%
%%
%%


%% 怎么响应前端数据
%% 心跳包