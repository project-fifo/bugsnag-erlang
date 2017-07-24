-module(bugsnag).
-behavior(gen_server).

-include("bugsnag.hrl").

-export([start/0, start_link/4, notify/5, notify/7, test_error/0,
         format/2,
         deliver_payload/1, process_trace/1, base/2, event_base/3]).

%% Gen server hooks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-ignore_xref([
              init/1,
              handle_call/3,
              handle_cast/2,
              handle_info/2,
              terminate/2,
              code_change/3,
              start/0, start_link/4, notify/5, notify/7, test_error/0,
              deliver_payload/1, process_trace/1]).

-record(state, {
          api_key,
          release_stage,
          app,
          app_version,
          client_version,
          hostname}).

base(ApiKey, Vsn) ->
    #{
      <<"apiKey">> => ApiKey,
      <<"notifier">> => #{
          <<"name">> => ?NOTIFIER_NAME,
          <<"version">> => Vsn,
          <<"url">> => ?NOTIFIER_URL
         }
     }.

event_base(ReleaseStage, AppVersion, HostName) ->
    #{
       <<"payloadVersion">> => <<"2">>,
       <<"context">> => HostName,
       <<"releaseStage">> => ReleaseStage,
       <<"app">> => #{
           <<"version">> => AppVersion
          },
       <<"device">> => #{<<"hostname">> => HostName}
     }.


%% Public API
start() ->
    inets:start(),
    crypto:start(),
    ssl:start(),
    lager:start(),
    application:start(bugsnag).

start_link(ApiKey, ReleaseStage, App, AppVersion) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [ApiKey, ReleaseStage, App, AppVersion], []).

notify(Type, Reason, Message, Module, Line) ->
    notify(Type, Reason, Message, Module, Line, generate_trace(), undefined).

notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    case erlang:process_info(whereis(?MODULE), message_queue_len) of
        {message_queue_len, N} when N > 100 ->
            %% if we got more then 100 messages queued up we simply
            %% stop writing
            ok;
        _ ->
            gen_server:cast(
              ?MODULE, {exception, Type, Reason, Message, Module, Line,
                        Trace, Request})
    end.

test_error() ->
    gen_server:cast(?MODULE, {test_error}).

%% Gen server hooks
init([ApiKey, ReleaseStage, App, AppVersion]) ->
    {ok, V} = application:get_key(bugsnag, vsn),
    HostName = atom_to_binary(node(), utf8),
    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage,
                client_version = list_to_binary(V),
                app = App, app_version = AppVersion, hostname = HostName}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

%% If no API key is defined we simply drop any exception.
handle_cast(_, State = #state{api_key = undefined}) ->
    {noreply, State};

handle_cast({exception, Type, Reason, Message, Module, Line, Trace, Request},
            State) ->
    send_exception(Type, Reason, Message, Module, Line, Trace, Request, State),
    {noreply, State};

handle_cast({test_error}, State) ->
    erlang:error(test_error),
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format(Term, Args) ->
    lager_format:format(Term, Args, ?FORMAT_LENGHT).


%% Internal API
send_exception(_Type, Reason, Message, Module, _Line, Trace, _Request,
               #state{api_key = APIKey, release_stage = ReleaseStage,
                      client_version = ClientVersion,
                      hostname = HostName, app_version= AppVersion}) ->
    MessageStr = case Message of
                     M when is_binary(M) ->
                         M;
                     Term ->
                         list_to_binary(format("~p", [Term]))
                 end,
    Base = base(APIKey, ClientVersion),
    EventBase0 =  event_base(ReleaseStage, AppVersion, HostName),
    EventBase = case application:get_application(Module) of
                    {ok, App} ->
                        EventBase0#{<<"groupingHash">> => atom_to_binary(App, utf8)};
                    _ ->
                        EventBase0
                end,
    ErrorClass = list_to_binary(format("~p", [Reason])),
    Payload =
        Base#{
          <<"events">> =>
              [EventBase#{
                 <<"exceptions">> =>
                     [#{
                         <<"errorClass">> => ErrorClass,
                         <<"message">> => MessageStr,
                         <<"stacktrace">> => process_trace(Trace)
                       }]}]
         },
    deliver_payload(jsone:encode(Payload)).


process_trace(undefined) ->
    [];
process_trace(Trace) ->
    process_trace(Trace, []).

process_trace([], ProcessedTrace) ->
    lists:reverse(ProcessedTrace);
process_trace([Current|Rest], ProcessedTrace) ->
    StackTraceLine = case Current of
                         {M, F, As, [{file, File}, {line, Line}]} ->
                             #{<<"file">> => list_to_binary(File),
                               <<"lineNumber">> => Line,
                               <<"method">> => fma_to_binary(F, M, As)};
                         {M, F, As, []} ->
                             #{<<"file">> => <<(atom_to_binary(M, utf8))/binary,
                                               ".erl">>,
                               <<"method">> => fma_to_binary(F, M, As)};

                         {M, F, As} when is_atom(M), is_atom(F), is_list(As) ->
                             #{<<"method">> => fma_to_binary(F, M, As)};
                         _ ->
                             lager:error("Discarding stack trace line: ~p",
                                         [Current]),
                             []
                     end,
    process_trace(Rest, [ StackTraceLine | ProcessedTrace]).

fma_to_binary(F, M, As) ->
    list_to_binary(format("~s:~s/~p", [M, F, As])).

deliver_payload(Payload) ->
    case httpc:request(post,
                       {?NOTIFY_ENDPOINT, [], "application/json", Payload},
                       [{timeout, 5000}], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            lager:debug("Error sent. Response: ~p", [Body]);
        {_, {{_Version, Status, ReasonPhrase}, _Headers, _Body}} ->
            lager:error("Failed to send error to bugsnag (~p : ~p)",
                        [Status, ReasonPhrase])
    end,
    ok.

generate_trace() ->
  lager:info("Generating trace"),
  try
    throw(bugsnag_gen_trace)
  catch bugsnag_gen_trace -> erlang:get_stacktrace()
  end.
