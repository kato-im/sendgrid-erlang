-module(esendgrid).

-export([send_email/1]).

parse_email(String) ->
    {ok, R0} = re:compile(<<"\([^<]+\) <\([^>]+\)>">>),
    case re:run(String, R0, [{capture, all_but_first, binary}]) of
        {match, [Name, Email]} ->
            {Name, Email};
        nomatch ->
            String
    end.

send_email(Json) when is_binary(Json) ->
    Jterm = jiffy:decode(Json),
    Subject = ej:get({"Subject"}, Jterm),
    Text = ej:get({"TextBody"}, Jterm),
    To = ej:get({"To"}, Jterm),
    From = ej:get({"From"}, Jterm),
    Params0 = [
        {<<"subject">>, Subject},
        {<<"text">>, Text}
    ],
    Params1 = case parse_email(To) of
        {ToName, ToEmail} ->
            Params0 ++ [ {<<"to">>, ToEmail}, {<<"toname">>, ToName} ];
        ToEmail ->
            Params0 ++ [ {<<"to">>, ToEmail} ]
    end,
    Params2 = case parse_email(From) of
        {FromName, FromEmail} ->
            Params1 ++ [ {<<"from">>, FromEmail}, {<<"fromname">>, FromName} ];
        FromEmail ->
            Params1 ++ [ {<<"from">>, FromEmail} ]
    end,
    handle_response(send_email(Params2));

send_email(Params) when is_list(Params) ->
    {ok, ApiUser} = application:get_env(esendgrid, sendgrid_api_user),
    {ok, ApiKey} = application:get_env(esendgrid, sendgrid_api_key),

    Auth = [
        {<<"api_user">>, ApiUser},
        {<<"api_key">>, ApiKey}
    ],
    Body = form_urlencode(Params ++ Auth),
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

handle_response({ok, {{200, _}, _, JsonResponse}}) ->
    {ok, jiffy:decode(JsonResponse)};

handle_response({ok, {{400, "Bad Request"}, _, JsonResponse}}) ->
    Jterm = jiffy:decode(JsonResponse),
    case ej:get({<<"message">>, <<"error">>, <<"errors">>, 0}, Jterm) of
        <<"Invalid from email address", _/binary>> ->
            {error, illegal_from_address};
        <<"Invalid to email address", _/binary>> ->
            {error, illegal_to_address};
        _ ->
            {error, Jterm}
    end;

handle_response({error, timeout}) ->
    {error, timeout}.

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
