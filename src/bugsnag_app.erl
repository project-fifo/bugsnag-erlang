-module(bugsnag_app).
-behavior(application).

%% Application hooks
-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting bugsnag notifier"),
    {ok, ReleaseStateS} = application:get_env(bugsnag, release_state),
    ReleaseState = list_to_binary(ReleaseStateS),

    {ok, AppS} = application:get_env(bugsnag, app),
    App = list_to_binary(AppS),

    {ok, AppVersionS} = application:get_env(bugsnag, app_version),
    AppVersion = list_to_binary(AppVersionS),
    case application:get_env(bugsnag, error_logger) of
        {ok, true} ->
            error_logger:add_report_handler(bugsnag_error_logger);
        _ -> ok
    end,

    case application:get_env(bugsnag, api_key) of
        {ok, ApiKeyS} ->
            ApiKey = list_to_binary(ApiKeyS),
            bugsnag_sup:start_link(ApiKey, ReleaseState, App, AppVersion);
        undefined ->
            bugsnag_sup:start_link(undefined, ReleaseState, App, AppVersion)
    end.

stop(_State) ->
    lager:info("Stopping bugsnag notifier"),
    ok.
