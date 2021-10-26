%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(conbee).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------



%% External exports
-export([start/0,
	get_temp/0,get_door/0,get_visitor/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()-> 

    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
-define(ConbeeAddr,"192.168.0.100").
-define(ConbeePort,80).
-define(Crypto,"0BDFAC94EE").
-define(Temp,"/api/0BDFAC94EE/sensors/14").
-define(Door,"/api/0BDFAC94EE/sensors/11").
-define(Visitor,"/api/0BDFAC94EE/sensors/12").


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_temp()->
    {ok, ConnPid} = gun:open(?ConbeeAddr,?ConbeePort),
    Ref=gun:get(ConnPid,?Temp),
    Result= get_temp(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.
get_temp({ok,Body})->
    get_temp(Body);
get_temp(Body)->
     X1=jsx:decode(Body,[]),
    true=is_map(X1),
    Z=maps:get(<<"state">>,X1),
    true=is_map(Z),
    Temp=maps:get(<<"temperature">>,Z),
    float_to_list(Temp/100,[{decimals,1}]).   

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_door()->
    {ok, ConnPid} = gun:open(?ConbeeAddr,?ConbeePort),
    Ref=gun:get(ConnPid,?Door),
    Result= get_door(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.


get_door({ok,Body})->
    get_door(Body);
get_door(Body)->
    X1=jsx:decode(Body,[]),
    true=is_map(X1),
    Z=maps:get(<<"state">>,X1),
    true=is_map(Z),
    case maps:get(<<"open">>,Z) of
	true->
	    "door is open";
	false ->
	    "door is closed"
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_visitor()->
    {ok, ConnPid} = gun:open(?ConbeeAddr,?ConbeePort),
    Ref=gun:get(ConnPid,?Visitor),
    Result= get_visitor(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.

get_visitor({ok,Body})->
    get_visitor(Body);
get_visitor(Body)->
     X1=jsx:decode(Body,[]),
    true=is_map(X1),
    Z=maps:get(<<"state">>,X1),
    true=is_map(Z),
    case maps:get(<<"presence">>,Z) of
	true->
	    "someone is presence";
	false ->
	    "noone is here"
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
