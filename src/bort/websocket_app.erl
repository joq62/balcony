%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    Dir=web_example,
    HtmlFile="index.html",
    HelloRoute = { "/", cowboy_static, {priv_file,Dir, HtmlFile} },
    WebSocketRoute = {"/please_upgrade_to_websocket", balcony_handler, []},
    CatchallRoute = {"/[...]", no_matching_route_handler, []},

    Dispatch = cowboy_router:compile([
				      {'_',
				       [HelloRoute, 
					WebSocketRoute, 
					CatchallRoute
				       ]
				      }
				     ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
							 env => #{dispatch => Dispatch}
							}),
    R=websocket_sup:start_link(),
    io:format("R  ~p~n",[{?MODULE,?LINE,R}]),
    R.

stop(_State) ->
    ok = cowboy:stop_listener(http).
