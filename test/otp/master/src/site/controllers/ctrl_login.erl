%%%-------------------------------------------------------------------
%%% @author yilin.jiang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 五月 2018 15:26
%%%-------------------------------------------------------------------
-module(ctrl_login).
-author("yilin.jiang").
-include("master.hrl").

%% API
-export([check_login/2, nickname/0]).

check_login(Username, PasswordMd5) ->
    ?INFO_LOG("Username:~p, PasswordMd5:~p", [Username, PasswordMd5]),
    Users = master_lib:get_cfg_master_users(),
    case proplists:get_value(Username, Users) of
        undefined ->
            ?ERROR_LOG("username:~p is not exist", [Username]),
            false;
        Password ->
            case lib_base:md5(Password) of
                PasswordMd5 ->
                    ?INFO_LOG("user ~s login ok", [Username]),
                    {true, Username};
                PasswordMd5True ->
                    ?ERROR_LOG("PasswordMd5True:~p, PasswordMd5:~p", [PasswordMd5True, PasswordMd5]),
                    false
            end
    end.

nickname() ->
    wf:session(nickname).

