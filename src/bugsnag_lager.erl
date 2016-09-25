-module(bugsnag_lager).
-behaviour(gen_event).

-include("bugsnag.hrl").

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          api_key, release_stage, app, app_version, hostname,
          level
         }).

-include_lib("lager/include/lager.hrl").

%% @private
init([ApiKeyS, ReleaseStageS, AppS, AppVersionS, Level]) ->
    ApiKey = list_to_binary(ApiKeyS),
    ReleaseStage = list_to_binary(ReleaseStageS),
    App = list_to_binary(AppS),
    AppVersion = list_to_binary(AppVersionS),

    {ok, HostnameS} = inet:gethostname(),
    HostName = list_to_binary(HostnameS),

    {ok, #state{api_key = ApiKey, release_stage = ReleaseStage,
                level = lager_util:config_to_mask(Level),
                app = App, app_version = AppVersion, hostname = HostName}}.


%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};

handle_call({set_loglevel, Level}, State) ->
    try lager_util:config_to_mask(Level) of
        Lvl ->
            {ok, ok, State#state{level=Lvl}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;

handle_call(_Request, State) ->
    {ok, ok, State}.

base(#state{api_key = ApiKey}) ->
    bugsnag:base(ApiKey).

event_base(LevelStr,
           #state{release_stage = ReleaseStage,
                  %% TODO: This Level is probably wrong
                  app_version = AppVersion,
                  hostname = HostName}) ->
    Base = bugsnag:event_base(ReleaseStage, AppVersion, HostName),
    Base#{
      <<"severity">> => LevelStr
     }.

%% @private
handle_event({log, Level, {_Date, _Time}, [LevelStr, Location, Message]},
             %% TODO: This Level is probably wrong
             #state{level = LogLevel} = State) when Level =< LogLevel ->
    Base = base(State),
    EventBase = event_base(LevelStr, State),
    Payload =
        Base#{
          <<"events">> =>
              [EventBase#{
                 <<"exceptions">> =>
                     [#{
                         <<"message">> => Message,
                         <<"stacktrace">> => bugsnag:process_trace(Location)
                       }]}]
         },
    bugsnag:deliver_payload(jsone:encode(Payload)),
    {ok, State};

handle_event({log, Message},
             %% TODO: This Level is probably wrong
             #state{level = LogLevel} = State) ->
    case lager_util:is_loggable(Message, LogLevel, bugsnag) of
        true ->
            M = lager_msg:metadata(Message),
            LevelStr = atom_to_binary(lager_msg:severity(Message), utf8),
            File = case {v(file, M), v(module, M)} of
                       {undefined, undefined} -> undefined;
                       {undefined, Mod} -> list_to_binary(atom_to_list(Mod)
                                                          ++ ".erl");
                       {F, _} -> list_to_binary(F)
                   end,
            Base = base(State),
            EBase = event_base(LevelStr, State),
            R = case {File, v(line, M), v(function, M)} of
                    {undefined, _, _} -> ok;
                    {_, undefined, _} -> ok;
                    {File1, Line, undefined} ->
                        {ok,
                         EBase#{
                           <<"exceptions">> =>
                               [#{<<"message">> => lager_msg:message(Message),
                                  <<"stacktrace">> => #{
                                      <<"file">> => File1,
                                      <<"line">> => Line
                                     }}]
                          }};
                    {File1, Line, Func} ->
                        {ok,
                         EBase#{
                           <<"exceptions">> =>
                               [#{<<"message">> => lager_msg:message(Message),
                                  <<"stacktrace">> => #{
                                      <<"file">> => File1,
                                      <<"method">> => Func,
                                      <<"line">> => Line
                                     }}]
                          }}
                end,
            case R of
                {ok, Event} ->
                    Payload = Base#{<<"events">> => Event},
                    bugsnag:deliver_payload(jsone:encode(Payload));
                _ ->
                    ok
            end,
            {ok, State};
        false ->
            {ok, State}
    end;

handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

v(Key, List) ->
    v(Key, List, undefined).

v(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.
