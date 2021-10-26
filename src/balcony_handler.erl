-module(balcony_handler).

-export([actual_temp/2,
	 actual_door/2,actual_visitor/2]).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).


actual_temp(T,PidWebSocket)->
   % io:format("~p~n",[{T,PidWebSocket,?MODULE,?FUNCTION_NAME,?LINE}]),
    PidWebSocket!{actual_temp,T},
    ok.

actual_door(S,PidWebSocket)->
    PidWebSocket!{actual_door,S},
    ok.
actual_visitor(V,PidWebSocket)->
    PidWebSocket!{actual_visitor,V},
    ok.


init(Req, State) ->
    io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,State}]),
    {cowboy_websocket, Req,State}.

%% web interface MM to server
websocket_init(State) ->
    S=self(),
    io:format("~p~n",[{S,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ActualTemp, ActualDoor,ActualVisitor,WantedTemp}=balcony:pid(S),
    NewState=lists:append([{temp,ActualTemp},{door,ActualDoor},
			   {visitor,ActualVisitor},{wanted_temp,WantedTemp}],State), 
    {reply, {text,io_lib:format("~s~s~s~s~s~s~s", [ActualTemp,",",ActualDoor,",",ActualVisitor,",",WantedTemp])},NewState }.


websocket_handle({text, <<"decrease_temp">>}, State) ->
    
    NewTemp=balcony:decrease_temp(),
    io:format("decrease_temp ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    NewState=lists:keyreplace(wanted_temp,1, State, {wanted_temp,NewTemp}),
    {temp,Temp}=lists:keyfind(temp,1,NewState),
    {door,Door}=lists:keyfind(door,1,NewState),
    {visitor,Visitor}=lists:keyfind(visitor,1,NewState),
    {wanted_temp,WantedTemp}=lists:keyfind(wanted_temp,1,NewState),    
    {reply, {text,io_lib:format("~s~s~s~s~s~s~s", [Temp,",",Door,",",Visitor,",",WantedTemp])},NewState };

websocket_handle({text, <<"increase_temp">>}, State) ->
    NewTemp=balcony:increase_temp(),
    io:format("increase_temp ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    NewState=lists:keyreplace(wanted_temp,1, State, {wanted_temp,NewTemp}),
    {temp,Temp}=lists:keyfind(temp,1,NewState),
    {door,Door}=lists:keyfind(door,1,NewState),
    {visitor,Visitor}=lists:keyfind(visitor,1,NewState),
    {wanted_temp,WantedTemp}=lists:keyfind(wanted_temp,1,NewState),    
    {reply, {text,io_lib:format("~s~s~s~s~s~s~s", [Temp,",",Door,",",Visitor,",",WantedTemp])},NewState };


websocket_handle(Other, State) ->  %Ignore
    io:format("[Other,State~p~n",[{?MODULE,?LINE,Other,State}]),
    {ok, State}.


%%- Interface to server Middle man

websocket_info( {actual_temp,T}, State) ->
    io:format("~p~n",[{temp,T,?MODULE,?LINE}]),
    NewState=lists:keyreplace(temp,1, State, {temp,T}),
    {temp,Temp}=lists:keyfind(temp,1,NewState),
    {door,Door}=lists:keyfind(door,1,NewState),
    {visitor,Visitor}=lists:keyfind(visitor,1,NewState),
    {wanted_temp,WantedTemp}=lists:keyfind(wanted_temp,1,NewState),
    {reply, {text,io_lib:format("~s~s~s~s~s~s~s", [Temp,",",Door,",",Visitor,",",WantedTemp])},NewState };

websocket_info( {actual_door,S}, State) ->
    io:format("~p~n",[{door,S,?MODULE,?LINE}]),
    NewState=lists:keyreplace(door,1, State, {door,S}),
   {temp,Temp}=lists:keyfind(temp,1,NewState),
    {door,Door}=lists:keyfind(door,1,NewState),
    {visitor,Visitor}=lists:keyfind(visitor,1,NewState),
    {wanted_temp,WantedTemp}=lists:keyfind(wanted_temp,1,NewState),
    {reply, {text,io_lib:format("~s~s~s~s~s~s~s", [Temp,",",Door,",",Visitor,",",WantedTemp])},NewState };

websocket_info( {actual_visitor,V}, State) ->
    io:format("~p~n",[{visitor,V,?MODULE,?LINE}]),
    NewState=lists:keyreplace(visitor,1, State, {visitor,V}),
    {temp,Temp}=lists:keyfind(temp,1,NewState),
    {door,Door}=lists:keyfind(door,1,NewState),
    {visitor,Visitor}=lists:keyfind(visitor,1,NewState),
    {wanted_temp,WantedTemp}=lists:keyfind(wanted_temp,1,NewState),
   {reply, {text,io_lib:format("~s~s~s~s~s~s~s", [Temp,",",Door,",",Visitor,",",WantedTemp])},NewState };


websocket_info({text, Text}, State) ->
    {reply, {text, Text}, State};

    
websocket_info(Other, State) ->
    io:format("[Other,State~p~n",[{?MODULE,?LINE,Other,State}]),
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
