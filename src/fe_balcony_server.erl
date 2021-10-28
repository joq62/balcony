%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point 
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(fe_balcony_server).  
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {pid,
		wanted_temp,
		actual_temp,
	        actual_door,
	        actual_motion}).



%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(CheckIntervall,20*1000).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

-export([check/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%%-----------------------------------------------------------------------

%%----------------------------------------------------------------------
check()-> 
    gen_server:cast(?MODULE, {check}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    Temp="42.0",Door="closed",Motion="inget av barnen",WTemp="20.0",
    
    {ok, #state{wanted_temp=WTemp,
		actual_temp=Temp,
	        actual_door=Door,
	        actual_motion=Motion}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({websocket_init,Pid},_From,State) ->
    {Reply,NewState}=format_text(init,State#state{pid=Pid}),
    {reply, Reply,NewState};


handle_call({websocket_handle,{text, <<"decrease_temp">>}},_From,State) ->
    NewWantedTemp=list_to_float(State#state.wanted_temp)-0.5,
    NewWantedTempStr=float_to_list(NewWantedTemp,[{decimals,1}]),
    {Reply,NewState}=format_text(wanted_temp,NewWantedTempStr,State),
    {reply, Reply, NewState};


handle_call({websocket_handle,{text, <<"increase_temp">>}},_From,State) ->
    NewWantedTemp=list_to_float(State#state.wanted_temp)+0.5,
    NewWantedTempStr=float_to_list(NewWantedTemp,[{decimals,1}]),
    {Reply,NewState}=format_text(wanted_temp,NewWantedTempStr,State),
    {reply, Reply, NewState};


handle_call({wanted_temp,T},_From,State) ->
    WTemp=float_to_list(T,[{decimals,1}]),
  %  io:format("~p~n",[{wanted_temp,WTemp,?MODULE,?FUNCTION_NAME,?LINE}]),
    Reply=case is_pid(State#state.pid) of
	      false->
		  NewState=State,
		  {error,[not_initiated,?MODULE,?FUNCTION_NAME,?LINE]};
	      true->
		  {Msg,NewState}=format_text(wanted_temp,WTemp,State),
		  State#state.pid!Msg,
		  ok
	  end,
    {reply, Reply, NewState};

handle_call({door,S},_From,State) ->
    Reply=case is_pid(State#state.pid) of
	      false->
		  NewState=State,
		  {error,[not_initiated,?MODULE,?FUNCTION_NAME,?LINE]};
	      true->
		  {Msg,NewState}=format_text(door,S,State),
		  State#state.pid!Msg,
		  ok
	  end,
    {reply, Reply, NewState};

handle_call({motion,S},_From,State) ->
     Reply=case is_pid(State#state.pid) of
	      false->
		  NewState=State,
		  {error,[not_initiated,?MODULE,?FUNCTION_NAME,?LINE]};
	      true->
		  {Msg,NewState}=format_text(motion,S,State),
		  State#state.pid!Msg,
		  ok
	  end,
    {reply, Reply, NewState};

handle_call({temp,S},_From,State) ->
    WTemp=float_to_list(S,[{decimals,1}]),
     Reply=case is_pid(State#state.pid) of
	      false->
		  NewState=State,
		  {error,[not_initiated,?MODULE,?FUNCTION_NAME,?LINE]};
	      true->
		  {Msg,NewState}=format_text(temp,WTemp,State),
		  State#state.pid!Msg,
		  ok
	  end,
    {reply, Reply, NewState};

handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({check}, State) ->
    ActualTemp=rpc:call(node(),conbee,get_temp,[]),
    ActualDoor=rpc:call(node(),conbee,get_door,[]),
    ActualVisitor=rpc:call(node(),conbee,get_visitor,[]),

    case is_pid(State#state.pid) of
	false->
	    ok;
	true->
	    case ActualTemp=:=State#state.actual_temp of
		true->
		    ok;
		false->
		    balcony_handler:actual_temp(ActualTemp,State#state.pid)
	    end,
	    case ActualDoor=:=State#state.actual_door of
		true->
		    ok;
		false->
		    balcony_handler:actual_door(ActualDoor,State#state.pid)
	    end,
	    case ActualVisitor=:=State#state.actual_motion of
		true->
		    ok;
		false->
		    balcony_handler:actual_visitor(ActualVisitor,State#state.pid)
	    end
    end,
    NewState=State#state{actual_temp=ActualTemp,
			 actual_door=ActualDoor,
			 actual_motion=ActualVisitor},
    spawn(fun()->do_check() end),
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({gun_response,_X1,_X2,_X3,_X4,_X5}, State) ->
    {noreply, State};

handle_info({gun_up,_,_}, State) ->
    {noreply, State};


handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
do_check()->
    timer:sleep(?CheckIntervall),
    rpc:cast(node(),?MODULE,check,[]).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
format_text(init,State)->
    format_text(State).

format_text(wanted_temp,WTemp,State)->
    format_text(State#state{wanted_temp=WTemp});

format_text(door,S,State)->
    format_text(State#state{actual_door=S});  

format_text(motion,S,State)->
    format_text(State#state{actual_motion=S});

format_text(temp,S,State)->
    format_text(State#state{actual_temp=S}).

format_text(NewState)->
    Type=text,
    M=io_lib,
    F=format,
    Temp=NewState#state.actual_temp,
    Door=NewState#state.actual_door,
    Motion=NewState#state.actual_motion,
    WTemp=NewState#state.wanted_temp,
    A=["~s~s~s~s~s~s~s", [Temp,",",Door,",",Motion,",",WTemp]],
    {{ok,Type,M,F,A},NewState}.
