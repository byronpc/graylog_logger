%%%-------------------------------------------------------------------
%% @doc graylog_logger top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(graylog_logger_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => graylog_logger, start => {graylog_logger, init, []}}
    ],
    {ok, {SupFlags, ChildSpecs}}.

