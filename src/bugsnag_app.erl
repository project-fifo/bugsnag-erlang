-module(bugsnag_app).
-behavior(application).

%% Application hooks
-export([start/2, stop/1]).
-ignore_xref([start/2, stop/1]).

erlang_app_vsn(App) ->
    AppA = list_to_atom(App),
    Apps = application:loaded_applications(),
    case lists:keyfind(AppA, 1, Apps) of
        {AppA, _, Vsn} ->
            list_to_binary(Vsn);
        _ ->
            <<"">>
    end.

app_vsn(App) ->
    case application:get_env(bugsnag, app_version) of
        {ok, Vsn} when Vsn =/= "" ->
            list_to_binary(Vsn);
        _ ->
            erlang_app_vsn(App)
    end.

start(_Type, _Args) ->
    lager:info("Starting bugsnag notifier"),
    ReleaseStateS = application:get_env(bugsnag, release_state, ""),
    ReleaseState = list_to_binary(ReleaseStateS),

    AppS = application:get_env(bugsnag, app, ""),
    App = list_to_binary(AppS),

    AppVersion = app_vsn(AppS),
    ApiKey = case application:get_env(bugsnag, api_key) of
                 {ok, ApiKeyS} ->
                     list_to_binary(ApiKeyS);
                 undefined ->
                     undefined
             end,
    case {ApiKey, application:get_env(bugsnag, error_logger)} of
        {undefined, _} ->
            %% If we don't hae an API key we don't need to bother
            %% the error logger
            ok;
        {_, {ok, true}} ->
            error_logger:add_report_handler(bugsnag_error_logger);
        _ ->
            ok
    end,
    bugsnag_sup:start_link(ApiKey, ReleaseState, App, AppVersion).

stop(_State) ->
    lager:info("Stopping bugsnag notifier"),
    ok.
