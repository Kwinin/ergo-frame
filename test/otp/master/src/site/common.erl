%% -*- mode: nitrogen -*-
-module(common).
-export([try_template/1, menu/0, header/0, footer/0]).
-export([alternate_case/2, alternate_color/2, alternate_color_1/2, alternate_color_2/2]).
-include_lib("wf.hrl").

try_template(Url) ->
    case wf:user() of
        undefined -> wf:redirect_to_login("/login");
        _ -> #template{file = Url}
    end.

menu() ->
    #template{file = "./templates/common/menu.html"}.

header() ->
    #template{file = "./templates/common/header.html"}.

footer() ->
    #template{file = "./templates/common/footer.html"}.

%%% ALTERNATE CASE %%%
alternate_case(DataRow, Acc) when Acc == []; Acc == odd  ->
    [Title, Author, Description, Postback] = DataRow,
    F = fun string:to_upper/1,
    { [F(Title), F(Author), F(Description), Postback], even, [] };

alternate_case(DataRow, Acc) when Acc == even  ->
    [Title, Author, Description, Postback] = DataRow,
    F = fun string:to_lower/1,
    { [F(Title), F(Author), F(Description), Postback], odd, [] }.

%%% ALTERNATE BACKGROUND COLORS %%%
alternate_color(DataRow, Acc) when Acc == []; Acc==odd ->
    {DataRow, even, {top@style, "background-color: #eee;"}};

alternate_color(DataRow, Acc) when Acc == even ->
    {DataRow, odd, {top@style, "background-color: #ddd;"}}.

alternate_color_1(DataRow, _Acc) ->
    {DataRow, even, {top@style, "background-color: #eee;"}}.
alternate_color_2(DataRow, _Acc) ->
    {DataRow, odd, {top@style, "background-color: #ddd;"}}.