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
-module(balcony_server).  
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
	        actual_visitor}).



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
    % call gun_client 
    {ok,_}=gun_client_server:start(),
    ActualTemp=rpc:call(node(),conbee,get_temp,[]),
%    io:format("ActualTemp ~p~n",[{ActualTemp,?MODULE,?LINE}]),
    ActualDoor=rpc:call(node(),conbee,get_door,[]),
%    io:format("ActualDoor ~p~n",[{ActualDoor,?MODULE,?LINE}]),
    ActualVisitor=rpc:call(node(),conbee,get_visitor,[]),
%    io:format("ActualVisitor ~p~n",[{ActualVisitor,?MODULE,?LINE}]),
    WantedTemp="20.0",

    spawn(fun()->do_check() end),
    
    {ok, #state{actual_temp=ActualTemp,
	        actual_door=ActualDoor,
		actual_visitor=ActualVisitor,
		wanted_temp=WantedTemp}}.
    
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
handle_call({decrease_temp},_From,State) ->
    NewWantedTemp=list_to_float(State#state.wanted_temp)-0.5,
    Reply=float_to_list(NewWantedTemp,[{decimals,1}]),
    {reply, Reply, State#state{wanted_temp=Reply}};
handle_call({increase_temp},_From,State) ->
    NewWantedTemp=list_to_float(State#state.wanted_temp)+0.5,
    Reply=float_to_list(NewWantedTemp,[{decimals,1}]),
    {reply, Reply, State#state{wanted_temp=Reply}};


handle_call({temp,T},_From,State) ->
    io:format("~p~n",[{temp,T,?MODULE,?FUNCTION_NAME,?LINE}]),
    Reply=balcony_handler:actual_temp(T,State#state.pid),
    {reply, Reply, State};

handle_call({door,S},_From,State) ->
    Reply=balcony_handler:actual_door(S,State#state.pid),
    {reply, Reply, State};

handle_call({visitor,S},_From,State) ->
    Reply=balcony_handler:actual_visitor(S,State#state.pid),
    {reply, Reply, State};


handle_call({pid,Pid},_From,State) ->    
  %  io:format("~p~n",[{pid,Pid,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewState=State#state{pid=Pid},
    Reply={State#state.actual_temp,
	   State#state.actual_door,
	   State#state.actual_visitor,
	   State#state.wanted_temp},
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
	    case ActualVisitor=:=State#state.actual_visitor of
		true->
		    ok;
		false->
		    balcony_handler:actual_visitor(ActualVisitor,State#state.pid)
	    end
    end,
    NewState=State#state{actual_temp=ActualTemp,
			 actual_door=ActualDoor,
			 actual_visitor=ActualVisitor},
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
