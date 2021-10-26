-module(websocket_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("[ME]Entered websocket_handler~n"),
    Body = my_get_file("index.html"),  %% Body can be a binary() or an iolist() (which is a list containing integers, strings, or binaries)
    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			   Body,
			   Req0),
    
     io:format("Req  ~p~n",[{?MODULE,?LINE,Req}]),
    {ok, Req, State}.

my_get_file(Path) ->
    PrivDir = code:priv_dir(web1),  %% Finds the path of an application's priv directory
    AbsPath = filename:join([PrivDir, Path]),
%    io:format("Path  ~p~n",[{?MODULE,?LINE,Path}]),
 %   io:format("PrivDir  ~p~n",[{?MODULE,?LINE,PrivDir}]),
  %  io:format("AbsPath  ~p~n",[{?MODULE,?LINE,AbsPath}]),
   % io:format(" ~p~n",[{?MODULE,?LINE,file:list_dir(".")}]),
    case file:read_file(AbsPath) of 
	{ok, Bin} -> 
	    io:format("Bin  ~p~n",[{?MODULE,?LINE,Bin}]),
	    io:format("AbsPath  ~p~n",[{?MODULE,?LINE,AbsPath}]),
	    Bin;
	_  -> ["<div>Cannot read file: ", Path, "</div>"]  %% iolist()
    end.
