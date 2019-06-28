%% @doc dgram_logger top level supervisor.
%% @end


-module(dgram_logger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% API functions

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Supervisor callbacks

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {#{strategy => one_for_all,
                   intensity => 10,
                   period => 60}, 
                  [
                   #{id => dgram_logger,
                     start => {dgram_logger, start_link, []}}
                  ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
