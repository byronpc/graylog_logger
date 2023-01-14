%%%-------------------------------------------------------------------
%% @doc log public API
%% @end
%%%-------------------------------------------------------------------

-module(graylog_logger).

-export([
    % adding_handler/1,
    init/0,
    log/2
]).

-export([
    init/1,
    handle_event/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

log(LogEvent, Config) ->
    gen_event:notify(?MODULE, LogEvent).

init() ->
    Result = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(?MODULE, ?MODULE, []),
    Result.

init(_) ->
    Env = application:get_env(kernel, logger, []),
    {handler, _, _, #{host := Host, port := Port} = Config} = lists:keyfind(graylog_logger, 3, Env),
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    {ok, Address} = inet:getaddr(Host, inet),
    State = maps:merge(Config, #{
        socket => Socket,
        address => Address,
        local => graylog_logger_utils:hostname()}),
    {ok, State}.

handle_event(Event, #{socket := Socket, host := Host, port := Port} = State) ->
    Message = catch graylog_logger_gelf_formatter:format(Event, State),
    gen_udp:send(Socket, Host, Port, Message),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
