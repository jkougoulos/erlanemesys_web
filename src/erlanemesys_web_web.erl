%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for erlanemesys_web.

-module(erlanemesys_web_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "del/" ++ Host ->
                                ok = ping_mgr:del( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to delete: " ++ Host ++ "<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "add/" ++ Host ->
                                {ok, Pid } = ping_mgr:add( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to add: " ++ Host ++
                                   " ok, Pid is "  ++ pid_to_list( Pid ) ++ "!<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "run/" ++ Host ->
                                ok = ping_mgr:run( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to run: " ++ Host ++ " ok!<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "report/" ++ Host ->
                                { ok, Str }  = ping_stats:report( Host ),
                                 Req:respond({200, [{"Content-Type", "text/plain"}],
                                   Str ++ "!\n"});
                    "sleep/" ++ Host ->
                                ok = ping_mgr:sleep( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to put into sleep: " ++ Host ++ "<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>\n"
                                ++ "</HTML>"
                                });
                    "active" ->
                                Active = lists:sort( ping_mgr:get_active() ) ,
                                TheFormat = "<a href=\"/report/~s\">~s</a><BR>",
                                ActStr = lists:flatmap( fun(X) ->
                                                io_lib:format(TheFormat,[X,X]) end,Active),
                                Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "Current monitored: <BR>" ++ ActStr
                                   ++ "</HTML>"
                                });

                    "interval/" ++ Rest ->
                                Tokens = string:tokens( Rest, "/" ),
                                Host = lists:nth(1,Tokens),
                                Interval = lists:nth(2,Tokens),
                                ok = ping_mgr:set_interval( Host, Interval ),
                                 Req:respond({200, [{"Content-Type", "text/plain"}],
                                   "You asked to change interval: " ++ Host ++
                                   " interval " ++ Interval ++ " ok!\n"});
                    _ ->
                        Req:respond({200, [{"Content-Type", "text/plain"}],
                                   "Sorry, I don't understand your request: " ++ Path ++ "\n"
                                ++ "help: \n"
                                ++ "/add/IP|hostname   Monitor this host\n"
                                ++ "/run/IP|hostname   Start monitoring\n"
                                ++ "/sleep/IP|hostname   Stop monitoring\n"
                                ++ "/interval/IP|hostname/xxx   Send probe every xxx msec > 100 \n"
                                ++ "/report/IP|hostname   get results\n"
                                ++ "/active   get active hosts\n"
                        })

%                  "hello_world" ->
%                    Req:respond({200, [{"Content-Type", "text/plain"}],
%                    "Hello world!\n"});
%                    _ ->
%                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
