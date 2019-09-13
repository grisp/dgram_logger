-module(dgram_logger_test_utils).

-include_lib("kernel/include/logger.hrl").

% API
-export([notice/1]).
-export([temp/1]).

%--- API -----------------------------------------------------------------------

notice(String) -> ?LOG_NOTICE(String).

temp(Value) -> ?LOG_NOTICE(#{temperature => Value}).
