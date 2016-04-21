# esendgrid
An Erlang library for sending e-mails via SendGrid [Web API v2]
(https://sendgrid.com/docs/API_Reference/Web_API/mail.html)

Installing
----------

In rebar.config:

```Erlang
{esendgrid, ".*", {git, "https://github.com/kato-im/sendgrid-erlang.git", {tag, "master"}}}
```

In sys.config:

```Erlang
{esendgrid, [
  {sendgrid_api_user, <<"your-api-user">>},
  {sendgrid_api_key,  <<"your-api-key">>}
]}
```

Usage
-----

```Erlang
esendgrid:send_email([ {<<"to">>, <<"to@domain.com">>}
                     , {<<"from">>, <<"from@domain.com">>}
                     , {<<"subject">>, <<"Subject message">>}
                     , {<<"text">>, <<"Your body message here">>}
                     , {<<"html">>, <<"Or you could also add some html here instead of text">>}
                     ]).
```
