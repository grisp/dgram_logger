-module(dgram_logger_test_utils).

-include_lib("kernel/include/logger.hrl").

% API
-export([start/1]).
-export([start/2]).
-export([stop/1]).
-export([notice/1]).
-export([temp/1]).

-define(DEFAULT_CONFIG, #{host => {127,0,0,1}, port => 8089}).

%--- API -----------------------------------------------------------------------

start(ID) -> start(ID, #{}).

start(ID, Config) ->
    ok = logger:add_handler(ID, dgram_logger, #{
        config => maps:merge(?DEFAULT_CONFIG, Config)
    }).

stop(ID) ->
    ok = logger:remove_handler(ID).

notice(String) -> ?LOG_NOTICE(String).

temp(Value) -> ?LOG_NOTICE(#{temperature => Value}).
