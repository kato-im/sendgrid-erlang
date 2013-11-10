-module(esendgrid).

-export([send_email/1]).
-export([send_email/4]).

send_email(Json) ->
    Jterm = jiffy:decode(Json),
    To = ej:get({"To"}, Jterm),
    From = ej:get({"From"}, Jterm),
    Subject = ej:get({"Subject"}, Jterm),
    Text = ej:get({"Text"}, Jterm),
    send_email(To, From, Subject, Text).

send_email(To, From, Subject, Text)
  when is_binary(To), is_binary(From), is_binary(Subject), is_binary(Text) ->
    {ok, ApiUser} = application:get_env(esendgrid, sendgrid_api_user),
    {ok, ApiKey} = application:get_env(esendgrid, sendgrid_api_key),

    Params = [
        {<<"to">>, To},
        {<<"from">>, From},
        {<<"subject">>, Subject},
        {<<"text">>, Text},
        {<<"api_user">>, ApiUser},
        {<<"api_key">>, ApiKey}
    ],
    Body = form_urlencode(Params),
    Res = lhttpc:request(
        "https://sendgrid.com/api/mail.send.json",
        "POST",
        [
            {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
            {<<"Accept">>, <<"application/json">>}
        ],
        Body,
        5000
    ),
    Res.

form_urlencode(Proplist) ->
    form_urlencode(Proplist, []).

form_urlencode([], Acc) ->
    list_to_binary(string:join(lists:reverse(Acc), "&"));

form_urlencode([{Key, Value} | R], Acc) when is_binary(Key), is_integer(Value) ->
    form_urlencode([{binary_to_list(Key), integer_to_list(Value)} | R], Acc);

form_urlencode([{Key, Value} | R], Acc) when is_list(Key), is_integer(Value) ->
    form_urlencode([{Key, integer_to_list(Value)} | R], Acc);

form_urlencode([{Key, Value} | R], Acc) when is_binary(Key), is_binary(Value) ->
    form_urlencode([{binary_to_list(Key), binary_to_list(Value)} | R], Acc);

form_urlencode([{Key, Value} | R], Acc) when is_list(Key), is_list(Value) ->
    form_urlencode(R, [esc(Key) ++ "=" ++ esc(Value) | Acc]).

esc(S) -> http_uri:encode(S).
