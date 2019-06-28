-module(dgram_logger).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%% logger callbacks
-export([adding_handler/1, log/2]).

-record(state, {sock}).

%% logger callbacks

adding_handler(#{config := _C}=Config) -> 
    % add checks for internal config
    {ok, Config}.

log(Log_event, #{config := C}) ->
    case C of
        #{sock := Sock,
          host := Host,
          port := Port} ->
            send(Sock, Host, Port, Log_event)
    end.

%% logger helper functions

send(Sock, Host, Port, #{level := Level, msg := Msg}) ->            
    Data = case Msg of
               {string, String} ->
                   iolist_to_binary(
                     io_lib:format("sol,level=~p msg=\"~s\"\n", 
                                   [Level, String]));
               {report, Report} ->
                   iolist_to_binary(
                     io_lib:format("sol,level=~p msg=\"~s\"\n", 
                                   [Level, "report"]));
               {Format, Args} when is_list(Args) ->
                   Io_list = io_lib:format(Format, Args),
                   iolist_to_binary(
                     io_lib:format("sol,level=~p msg=\"~s\"\n", 
                                   [Level, Io_list]))
           end,
    gen_udp:send(Sock, Host, Port, Data).

%% gen_server callbacks

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, Sock} = gen_udp:open(0),
    ok = logger:add_handler(dgram_logger, dgram_logger, 
                            #{config => #{sock => Sock,
                                          host => {127,0,0,1}, 
                                          port => 8089}}),
    {ok, #state{sock=Sock}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
