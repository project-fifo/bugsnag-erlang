-module(bugsnag).
-behavior(gen_server).

-include("bugsnag.hrl").

-export([start/0, start_link/4, notify/5, notify/7, test_error/0,
         deliver_payload/1, process_trace/1, base/1, event_base/3]).

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

-record(state, {api_key, release_stage, app, app_version, hostname}).

base(ApiKey) ->
    #{
      <<"apiKey">> => ApiKey,
      <<"notifier">> => #{
          <<"name">> => ?NOTIFIER_NAME,
          <<"version">> => ?NOTIFIER_VERSION,
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
    notify(Type, Reason, Message, Module, Line, undefined, undefined).
notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    gen_server:cast(?MODULE, {exception, Type, Reason, Message, Module, Line,
                              Trace, Request}).

test_error() ->
    gen_server:cast(?MODULE, {test_error}).

%% Gen server hooks
init([ApiKey, ReleaseStage, App, AppVersion]) ->
    HostName = atom_to_binary(node(), utf8),
    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage,
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

%% Internal API
send_exception(_Type, Reason, Message, Module, _Line, Trace, _Request,
               #state{api_key = APIKey, release_stage = ReleaseStage,
                      hostname = HostName, app_version= AppVersion}) ->
    MessageStr = case Message of
                     M when is_binary(M) ->
                         M;
                     Term ->
                         list_to_binary(io_lib:format("~p", [Term]))
                 end,
    Base = base(APIKey),
    EventBase0 =  event_base(ReleaseStage, AppVersion, HostName),
    EventBase = case application:get_application(Module) of
                    {ok, App} ->
                        EventBase0#{<<"groupingHash">> => atom_to_binary(App, utf8)};
                    _ ->
                        EventBase0
                end,
    ErrorClass = list_to_binary(io_lib:format("~p", [Reason])),
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
                         {_, F, _, [{file, File}, {line, Line}]} ->
                             Method = list_to_binary(io_lib:format("~p", [F])),
                             #{<<"file">> => list_to_binary(File),
                               <<"lineNumber">> => Line,
                               <<"method">> => Method};
                         {_, F, _} ->
                             Method = list_to_binary(io_lib:format("~p", [F])),
                             #{<<"method">> => Method};
                         _ ->
                             lager:error("Discarding stack trace line: ~p",
                                         [Current]),
                             []
                     end,
    process_trace(Rest, [ StackTraceLine | ProcessedTrace]).


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
