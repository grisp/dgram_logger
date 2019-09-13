-module(dgram_logger).

%% logger callbacks
-export([adding_handler/1]).
-export([log/2]).
-export([removing_handler/1]).

%% logger callbacks

adding_handler(#{config := C} = Config) ->
    Pid = dgram_logger_sup:add_handler_proc(),
    {ok, Config#{config => C#{
        sock => dgram_logger_proc:get_socket(Pid),
        pid => Pid
    }}}.

log(Log_event, #{config := C}) ->
    case C of
        #{sock := Sock,
          host := Host,
          port := Port} ->
            send(Sock, Host, Port, Log_event)
    end.

removing_handler(#{config := #{pid := Pid}}) ->
    dgram_logger_sup:remove_handler_proc(Pid).

%% logger helper functions

send(Sock, Host, Port, #{level := Level, msg := Msg, meta := Meta}) ->
    %% erlang:display({meta, Meta}),
    %% erlang:display({msg, Msg}),
    Tags = meta_tags(Meta),
    Data = case Msg of
               {string, String} ->
                   iolist_to_binary(
                     io_lib:format("sol,level=~w~s msg=\"~s\"\n",
                                   [Level, Tags, String]));
               %% special treatmeant of SASL logs
               {report, #{label := Label, report := Report}} ->
                   iolist_to_binary(
                     io_lib:format("sol,level=~w~s ~s\n",
                                   [Level, Tags,
                                    report_values([Label|Report])]));
               {report, Report} ->
                   iolist_to_binary(
                     io_lib:format("sol,level=~w~s ~s\n",
                                   [Level, Tags, report_values(Report)]));
               {Format, Args} when is_list(Args) ->
                   Io_list = io_lib:format(Format, Args),
                   iolist_to_binary(
                     io_lib:format("sol,level=~w~s msg=\"~s\"\n",
                                   [Level, Tags, Io_list]))
           end,
    ok = gen_udp:send(Sock, Host, Port, Data).

meta_tags(Meta) ->
    prepend_comma(lists:join($,, maps:fold(fun format_tag/3, [], Meta))).

report_values(Report) when is_list(Report) ->
    lists:join($,, lists:foldl(fun format_entry/2, [], Report));
report_values(Report) when is_map(Report) ->
    lists:join($,, maps:fold(fun format_entry/3, [], Report)).

prepend_comma([]) ->
    [];
prepend_comma(List) ->
    [$,|List].

format_tag(mfa, {Module, Function, Arity}, Acc) ->
    [[
        ["module=", atom_to_list(Module), ","],
        ["function=", atom_to_list(Function), ","],
        ["arity=", io_lib:format("~b", [Arity])]
    ]|Acc];
format_tag(time, _, Acc) ->
    Acc;
format_tag(report_cb, _, Acc) ->
    Acc;
format_tag(error_logger, _, Acc) ->
    Acc;
format_tag(logger_formatter, _, Acc) ->
    Acc;
format_tag(domain, Dlist, Acc) ->
    ["domain=" ++
         lists:join($/, lists:map(fun erlang:atom_to_list/1, Dlist)) |Acc];
format_tag(Key, Value, Acc) ->
    case io_lib:deep_char_list(Value) of
        true -> [io_lib:format("~w=~s", [Key, Value]) | Acc];
        false -> [io_lib:format("~w=~w", [Key, Value]) | Acc]
    end.

format_entry({Key, Value}, Acc) ->
    format_entry(Key, Value, Acc);
format_entry(Any, Acc) ->
    format_entry(dummy, Any, Acc).

format_entry(Key, Value, Acc) when is_integer(Value) ->
    [io_lib:format("~w=~wi", [Key, Value]) | Acc];
format_entry(Key, Value, Acc) when is_boolean(Value); is_float(Value) ->
    [io_lib:format("~w=~w", [Key, Value]) | Acc];
format_entry(Key, Value, Acc) ->
    V = case io_lib:deep_char_list(Value) of
            true -> io_lib:format("~s", [Value]);
            false -> io_lib:format("~w", [Value])
        end,
    [io_lib:format("~w=\"~s\"", [Key, escape_chars(V, $\", $\\ )])  | Acc].

escape_chars([], _Q, _Esc) ->
    [];
escape_chars([Q|Chars], Q, Esc) ->
    [Esc,Q|escape_chars(Chars, Q, Esc)];
escape_chars([C|Chars], Q, Esc) when is_integer(C) ->
    [C|escape_chars(Chars, Q, Esc)];
escape_chars([L|Chars], Q, Esc) when is_list(L) ->
    [escape_chars(L, Q, Esc)|escape_chars(Chars, Q, Esc)].
