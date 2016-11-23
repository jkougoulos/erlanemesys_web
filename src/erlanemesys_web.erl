%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc erlanemesys_web.

-module(erlanemesys_web).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the erlanemesys_web server.
start() ->
    erlanemesys_web_deps:ensure(),
    ensure_started(crypto),
    application:start(erlanemesys_web).


%% @spec stop() -> ok
%% @doc Stop the erlanemesys_web server.
stop() ->
    application:stop(erlanemesys_web).
