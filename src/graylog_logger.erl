%%%-------------------------------------------------------------------
%% @doc log public API
%% @end
%%%-------------------------------------------------------------------

-module(graylog_logger).
-include_lib("kernel/include/logger.hrl").

-export([
    init/0,
    log/2
]).

-export([
    adding_handler/1,
    removing_handler/1,
    changing_config/3
]).

-export([
    init/1,
    handle_event/2
]).

adding_handler(Config) ->
    gen_event:add_handler(?MODULE, ?MODULE, []),
    {ok, Config}.

removing_handler(_Config) ->
    gen_event:delete_handler(?MODULE, ?MODULE, []),
    ok.

changing_config(update, _OldConfig, NewConfig) ->
    gen_event:notify(?MODULE, {config, NewConfig}),
    {ok, NewConfig};

changing_config(set, OldConfig, NewConfig) ->
    NewConfig2 = maps:merge(OldConfig, NewConfig),
    gen_event:notify(?MODULE, {config, NewConfig2}),
    {ok, NewConfig2}.

log(LogEvent, _Config) ->
    gen_event:notify(?MODULE, {log, LogEvent}).

init() ->
    Result = gen_event:start_link({local, ?MODULE}),
    logger:add_handlers(?MODULE),
    Result.

init(_) ->
    Env = application:get_env(graylog_logger, logger, []),
    {handler, _, _, #{host := Host} = Config} = lists:keyfind(graylog_logger, 3, Env),
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    {ok, Address} = inet:getaddr(Host, inet),
    State = maps:merge(Config, #{
        socket => Socket,
        address => Address,
        local => graylog_logger_utils:hostname()}),
    {ok, State}.

handle_event({config, Config}, State) ->
    State2 = maps:merge(State, Config),
    {ok, State2};

handle_event({log, Event}, #{socket := Socket, address := Address, port := Port} = State) ->
    Message = catch graylog_logger_gelf_formatter:format(Event, State),
    gen_udp:send(Socket, Address, Port, Message),
    {ok, State}.

