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
    Html = ej:get({"HtmlBody"}, Jterm),
    To = ej:get({"To"}, Jterm),
    From = ej:get({"From"}, Jterm),
    Params0 = [
        {<<"subject">>, Subject}
    ],
    Params1 = case Text of
        undefined ->
            Params0;
        Text when is_binary(Text) ->
            Params0 ++ [{<<"text">>, Text}]
    end,
    Params2 = case Html of
        undefined ->
            Params1;
        Html when is_binary(Html) ->
            Params1 ++ [{<<"html">>, Html}]
    end,
    %% TODO throw proper errror, must have either text or html
    true = Html =/= undefined orelse Text =/= undefined,
    Params3 = case parse_email(To) of
        {ToName, ToEmail} ->
            Params2 ++ [{<<"to">>, ToEmail}, {<<"toname">>, ToName}];
        ToEmail ->
            Params2 ++ [{<<"to">>, ToEmail}]
    end,
    Params4 = case parse_email(From) of
        {FromName, FromEmail} ->
            Params3 ++ [{<<"from">>, FromEmail}, {<<"fromname">>, FromName}];
        FromEmail ->
            Params3 ++ [{<<"from">>, FromEmail}]
    end,
    Params5 = case ej:get({"ReplyTo"}, Jterm) of
        undefined ->
            Params4;
        ReplyTo ->
            case parse_email(ReplyTo) of
                {_, ReplyToEmail} -> ReplyToEmail;
                ReplyToEmail -> ReplyToEmail
            end,
            Params4 ++ [{<<"replyto">>, ReplyToEmail}]
    end,
    handle_response(send_email(Params5));

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
    case ej:get({"errors", 1}, Jterm) of
        <<"Invalid from email address", _/binary>> ->
            {error, illegal_from_address};
        <<"Invalid email address", _/binary>> ->
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
