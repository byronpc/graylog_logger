-module(graylog_logger_gelf_formatter).

-export([format/2]).

-define(GELF_VERSION, <<"1.1">>).

format(Message, #{compression := Compression} = Config) ->
    do_compression(safe_encode(get_raw_data(Message, Config)), Compression).

% internals

safe_encode(Msg) ->
    case catch jiffy:encode(Msg, [force_utf8]) of
        JsonPayloadBin when is_binary(JsonPayloadBin) ->
            JsonPayloadBin;
        JsonPayloadList when is_list(JsonPayloadList) ->
            iolist_to_binary(JsonPayloadList);
        {error, _} ->
            {value, {_, _}, Msg1} = lists:keytake(<<"short_message">>, 1, Msg),
            safe_encode([{<<"short_message">>, <<"hex message failed to encode">>} | Msg1])
    end.

get_raw_data(#{level := Level, meta := #{time := Time} = Meta} = LogEvent, #{local := Host, extra_fields := ExtraFields, formatter := {FModule, FConfig}}) ->
    Meta2 = maps:merge(Meta, ExtraFields),
    Message = list_to_binary(FModule:format(LogEvent, FConfig)),
    maps:merge(#{
        version => ?GELF_VERSION,
        host => Host,
        timestamp => Time/1000000,
        level => graylog_logger_utils:severity2int(Level),
        short_message => Message
    }, get_metadata(Meta2)).

get_metadata(Meta) ->
    maps:map(fun(_K, V) -> graylog_logger_utils:term2bin(V) end, Meta).

do_compression(Data, gzip) ->
    zlib:gzip(Data);
do_compression(Data, zlib) ->
    zlib:compress(Data);
do_compression(Data, _) ->
    Data.