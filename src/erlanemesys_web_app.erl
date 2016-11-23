%% @author Mochi Media <dev@mochimedia.com>
%% @copyright erlanemesys_web Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the erlanemesys_web application.

-module(erlanemesys_web_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlanemesys_web.
start(_Type, _StartArgs) ->
    erlanemesys_web_deps:ensure(),
    erlanemesys_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlanemesys_web.
stop(_State) ->
    ok.
