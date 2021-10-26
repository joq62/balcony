-module(no_matching_route_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
 
init(Req, State) ->
    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,State}]),
    {cowboy_websocket, Req, State}.  %Perform websocket setup

%websocket_init(State) ->
 %   io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
  %  {ok, State}.
websocket_init(State) ->
%    Self=self(),
 %   spawn(fun()->tick(Self) end),
    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {temp,Temp}=lists:keyfind(temp,1,State),
    {reply, {text,io_lib:format("~s", ["error in no_amtching_route"])},State }.

websocket_handle({text, <<"decrease_temp">>}, State) ->
    {temp,Temp}=lists:keyfind(temp,1,State),
    NewTemp=Temp-1,
    NewState=lists:keyreplace(temp,1,State,{temp,NewTemp}),
    {reply, 
     {text, io_lib:format("~w", [NewTemp]) },
     NewState};

websocket_handle({text, <<"increase_temp">>}, State) ->
    {temp,Temp}=lists:keyfind(temp,1,State),
    NewTemp=Temp+1,
    NewState=lists:keyreplace(temp,1,State,{temp,NewTemp}),
    {reply, 
     {text, io_lib:format("~w", [NewTemp]) },
     NewState};



websocket_handle(Other, State) ->  %Ignore
    io:format("[Other,State~p~n",[{?MODULE,?LINE,Other,State}]),
    {ok, State}.


websocket_info({text, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Other, State) ->
    {ok, State}.

%% services 

action(Bin)->
    DoReply=true,
    Reply=case DoReply of
	      false->
		  'do someting with Bin, but no reply',
		  no_reply;
	      true->
		  Msg=binary_to_list(Bin),
		  io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Msg}]),
		  {Hours, Minutes, Secs} = time(),
		  ReplyInfo=io_lib:format("[~w:~w:~w]: Server received v2: ~s", [Hours, Minutes, Secs, Msg]),
		%  ReplyInfo=io_lib:format("~s,[~w:~w:~w]", ["time",Hours, Minutes, Secs]),
		  
    		  {reply,{text,ReplyInfo }}
	  end,
    Reply.
