%%% -------------------------------------------------------------------
%%% @author  : JoqErlang
%%% @version "1.0.0" 
%%% @since 2021-07-20
%%% @doc: logger support for joqs infrastructure
%%% @copyright : JoqErlang 
%%% -------------------------------------------------------------------
-module(balcony).   

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(SERVER,balcony_server).
%% --------------------------------------------------------------------
-export([
	 increase_temp/0,
	 decrease_temp/0,

	 temp/1,
	 door/1,
	 visitor/1,
	 pid/1,
	 ping/0
	]).

-export([
	 boot/0,
	 start/0,
	 stop/0
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals
boot()->
    ok=application:start(?MODULE).
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).




%%---------------------------------------------------------------
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

pid(Pid)-> 
    gen_server:call(?SERVER, {pid,Pid},infinity).
temp(T)-> 
    gen_server:call(?SERVER, {temp,T},infinity).
door(S)-> 
    gen_server:call(?SERVER, {door,S},infinity).
visitor(S)-> 
    gen_server:call(?SERVER, {visitor,S},infinity).
increase_temp()-> 
    gen_server:call(?SERVER, {increase_temp},infinity).
decrease_temp()-> 
    gen_server:call(?SERVER, {decrease_temp},infinity).





%%----------------------------------------------------------------------
