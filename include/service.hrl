%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 9月 2020 17:36
%%%-------------------------------------------------------------------
-author("Administrator").

-record(service, {
    id                  :: atom(),       %% 服务名称
    name = ""           :: bitstring(),  %% 服务的中文名称
    mfa                 :: undefined | {atom(), atom(), list()},
    restart = transient :: permanent | temporary | transient,
    type = worker       :: supervisor | worker,
    depend_on = []      :: list()
}).