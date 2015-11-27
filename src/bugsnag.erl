-module(bugsnag).
-behavior(gen_server).

-include("bugsnag.hrl").

-export([start/0, start_link/4, notify/5, notify/7, test_error/0,
         deliver_payload/1, process_trace/1]).

%% Gen server hooks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {api_key, release_stage, app, app_version, hostname}).

%% Public API
start() ->
    inets:start(),
    crypto:start(),
    ssl:start(),
    lager:start(),
    application:start(bugsnag).

start_link(ApiKey, ReleaseStage, App, AppVersion) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiKey, ReleaseStage, App, AppVersion], []).

notify(Type, Reason, Message, Module, Line) ->
    notify(Type, Reason, Message, Module, Line, undefined, undefined).
notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    gen_server:cast(?MODULE, {exception, Type, Reason, Message, Module, Line, Trace, Request}).

test_error() ->
    gen_server:cast(?MODULE, {test_error}).

%% Gen server hooks
init([ApiKey, ReleaseStage, App, AppVersion]) ->
    HostName = atom_to_binary(node(), utf8),
    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage,
                app = App, app_version = AppVersion, hostname = HostName}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({exception, Type, Reason, Message, Module, Line, Trace, Request}, State) ->
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
               _State = #state{api_key = APIKey, release_stage = ReleaStage,
                               hostname = HostName, app_version= AppVersion}) ->
    Grouping = case application:get_application(Module) of
                   {ok, App} ->
                       [{groupingHash, atom_to_binary(App, utf8)}];
                   _ ->
                       []
               end,
    MessageStr = case Message of
                     M when is_binary(M) ->
                         M;
                     Term ->
                         list_to_binary(io_lib:format("~p", [Term]))
                 end,
    Payload = [
               {apiKey, APIKey},
               {notifier, [
                           {name, ?NOTIFIER_NAME},
                           {version, ?NOTIFIER_VERSION},
                           {url, ?NOTIFIER_URL}
                          ]},
               {events, [
                         [
                          {payloadVersion, <<"2">>},
                          {context, HostName},
                          {releaseStage, ReleaStage},
                          {app, [
                                 {version, AppVersion}
                                ]},
                          {device, [{hostname, HostName}]},
                          {exceptions, [
                                        [
                                         {errorClass, list_to_binary(io_lib:format("~p", [Reason]))},
                                         {message, MessageStr},
                                         {stacktrace, process_trace(Trace)}
                                        ]
                                       ]}
                         ] ++ Grouping
                        ]}
              ],
    deliver_payload(jsx:encode(Payload)).


process_trace(undefined) -> [];
process_trace(Trace) ->
    lager:debug("Processing trace ~p", [Trace]),
    process_trace(Trace, []).

process_trace([], ProcessedTrace) -> ProcessedTrace;
process_trace([Current|Rest], ProcessedTrace) ->
    StackTraceLine = case Current of
                         {_, F, _, [{file, File}, {line, Line}]} ->
                             [
                              {file, list_to_binary(File)},
                              {lineNumber, Line},
                              {method, list_to_binary(io_lib:format("~p", [F]))}
                             ];
                         {_, F, _} ->
                             [
                              {method, list_to_binary(io_lib:format("~p", [F]))}
                             ];
                         _ ->
                             lager:error("Discarding stack trace line: ~p", [Current]),
                             []
                     end,
    process_trace(Rest, ProcessedTrace ++ [StackTraceLine]).


deliver_payload(Payload) ->
    lager:info("Sending exception: ~p", [Payload]),
    case httpc:request(post, {?NOTIFY_ENDPOINT, [], "application/json", Payload}, [{timeout, 5000}], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            lager:debug("Error sent. Response: ~p", [Body]);
        {_, {{_Version, Status, ReasonPhrase}, _Headers, _Body}} ->
            lager:error("Failed to send error to bugsnag (~p : ~p)", [Status, ReasonPhrase])
    end,
    ok.
