%% -*- mode: nitrogen -*-
-module(login).
-export([main/0, panel/0, event/1]).
-include_lib("../../../deps/nitrogen_core/include/wf.hrl").
-include("master.hrl").

main() ->
%%  wf:logout(),
    wf:clear_session(),
    #template{file = "./templates/login.html"}.

panel() ->
    #panel{
        body = [
            #textbox{
                id = username,
                class = 'form-control',
                placeholder = "用户名"
            },
            #password{
                id = password,
                class = 'form-control',
                placeholder = "密码"
            },
            #button{
                id = btnlogin,
                class = 'btn btn-lg btn-success btn-block',
                text = "登录",
                postback = {clicked, login}
            }
        ]
    }.

event({clicked, login}) ->
    ValueUsername = wf:q(username),
    ValuePassword = wf:q(password),
    Username = string:strip(ValueUsername),
    Password = string:strip(ValuePassword),
    ?INFO_LOG("Username:~p, Password:~p", [Username, Password]),
    PasswordMd5 = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(Password))]),
    case ctrl_login:check_login(Username, PasswordMd5) of
        {true, Username} ->
            wf:user(Username),
            wf:session(nickname, Username),
            wf:redirect_from_login("/index");
        false ->
            wf:wire(#alert{text = <<"用户名或密码错误"/utf8>>})
    end;
event(_) -> ok.