%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for erlanemesys_web.

-module(erlanemesys_web_web).
-author("John Kougoulos <john.kougoulos@gmail.com>").

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
                                ok = erlanemesys_api:del( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to delete: " ++ Host ++ "<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "addping/" ++ Host ->
                                {ok, Pid } = erlanemesys_api:addping( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to add ping: " ++ Host ++
                                   " ok, Pid is "  ++ pid_to_list( Pid ) ++ "!<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "addhttp/" ++ Host ->
                                {ok, Pid } = erlanemesys_api:addhttp( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to add http: " ++ Host ++
                                   " ok, Pid is "  ++ pid_to_list( Pid ) ++ "!<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "run/" ++ Host ->
                                ok = erlanemesys_api:run( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to run: " ++ Host ++ " ok!<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>"
                                ++ "</HTML>"
                                });
                    "sleep/" ++ Host ->
                                ok = erlanemesys_api:sleep( Host ),
                                 Req:respond({200, [{"Content-Type", "text/html"}],
                                   "<HTML>" ++
                                   "You asked to put into sleep: " ++ Host ++ "<BR>"
                                ++ "<a href=\"/active\"> Active ones</a><BR>\n"
                                ++ "</HTML>"
                                });
                    "active" ->
                                Active = lists:sort( erlanemesys_api:get_active() ) ,
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
                                ok = erlanemesys_api:set_interval( Host, Interval ),
                                 Req:respond({200, [{"Content-Type", "text/plain"}],
                                   "You asked to change interval: " ++ Host ++
                                   " interval " ++ Interval ++ " ok!\n"});
                    "report/" ++ Host ->
                                { ok, Str }  = erlanemesys_api:report( Host ),
                                 Req:respond({200, [{"Content-Type", "text/plain"}],
                                   Str ++ "!\n"});
                    _ ->
                        Req:respond({200, [{"Content-Type", "text/html"}],
				   "<HTML>" ++
                                   "Sorry, I don't understand your request: " ++ Path ++ "\n"
                                ++ "help: <BR>"
                                ++ "/addping/IP|hostname   Monitor this host<BR>"
                                ++ "/addhttp/IP|hostname   Monitor this host<BR>"
                                ++ "/run/IP|hostname   Start monitoring<BR>"
                                ++ "/sleep/IP|hostname   Stop monitoring<BR>"
                                ++ "/interval/IP|hostname/xxx   Send probe every xxx msec > 101 <BR>"
                                ++ "/report/IP|hostname   get results<BR>"
                                ++ "<a href=\"/active\"> /active   get active hosts<BR>"
                                ++ "</HTML>"
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
