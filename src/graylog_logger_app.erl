%%%-------------------------------------------------------------------
%% @doc graylog_logger public API
%% @end
%%%-------------------------------------------------------------------

-module(graylog_logger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    graylog_logger_sup:start_link().

stop(_State) ->
    ok.
