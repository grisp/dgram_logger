%% @doc dgram_logger top level supervisor.
%% @end


-module(dgram_logger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_handler_proc/0]).
-export([remove_handler_proc/1]).

%% Supervisor callbacks
-export([init/1]).

%% API functions

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_handler_proc() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    Pid.

remove_handler_proc(Pid) ->
    ok = supervisor:terminate_child(?MODULE, Pid).

%% Supervisor callbacks

init([]) ->
    {ok, {#{strategy => simple_one_for_one}, [
        #{id => dgram_logger_proc, start => {dgram_logger_proc, start_link, []}}
    ]}}.

