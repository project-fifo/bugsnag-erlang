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
