graylog_logger
================

This is a basic library that uses standard erlang logger (OTP21 onwards) to output messages to graylog via UDP protocol

Quick start
-----------

Include this library into your project using rebar:

```erl
{graylog_logger, ".*", {git, "https://github.com/byronpc/graylog_logger.git", "master"}}
```

Kernel Configuration

```erl
[{kernel,
  [
    {logger, [
      {handler, default, undefined},
      {handler, graylog, graylog_logger, #{
        host => "localhost",
        port => 12201,
        compression => disabled | gzip | zlib,
        extra_fields => #{
          <<"_environment">> => <<"production">>
        },
        formatter => {
          logger_formatter, #{
              single_line => true,
              time_offset => "Z",
              template => [msg]
          }
        }
      }}
    ]},
    {logger_level, warning}
  ]}
].
```
