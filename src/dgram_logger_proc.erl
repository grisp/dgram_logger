-module(dgram_logger_proc).

-behavior(gen_server).

% API
-export([start_link/0]).
-export([get_socket/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- API -----------------------------------------------------------------------

start_link() -> gen_server:start_link(?MODULE, undefined, []).

get_socket(Pid) -> gen_server:call(Pid, get_socket).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    {ok, Socket} = gen_udp:open(0),
    {ok, Socket}.

handle_call(get_socket, _From, Socket) ->
    {reply, Socket, Socket};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

