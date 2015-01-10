-module( cs_counter ).


-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
		start_link/2, start_link/3,
		register_counter/2, register_counter/3,
		unregister_counter/1, unregister_counter/2,
		stop/1, stop/2
	]).

-export([
		tick_counter/1, tick_counter/2,
		tick_counter_async/1, tick_counter_async/2
	]).

-export([
		get_value/1,
		get_active/1,
		get_name/1,
		get_description/1
	]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link( Parent, Name ) ->
	start_link( Parent, Name, [] ).

start_link( Parent, Name, Options ) ->
	gen_server:start_link( ?MODULE, [Parent, Name, Options], [] ).

register_counter( Parent, Name ) -> 
	cs_counter_sup:start_counter( Parent, Name ).

register_counter( Parent, Name, Options ) ->
	cs_counter_sup:start_counter( Parent, Name, Options ).

unregister_counter( Pid ) ->
	stop( Pid ).

unregister_counter( Pid, Reason ) ->
	stop( Pid, Reason ).

stop( Pid ) -> 
	stop( Pid, normal ).

stop( Pid, Reason ) ->
	gen_server:call( Pid, {stop, Reason}).

tick_counter( Pid ) ->
	tick_counter( Pid, 1 ).

tick_counter( Pid, Val ) ->
	gen_server:call( Pid, {tick_counter, Val}).

tick_counter_async( Pid ) ->
	tick_counter_async( Pid, 1 ).

tick_counter_async( Pid, Val ) -> 
	gen_server:cast( Pid, {tick_counter, Val}).
	
get_value( Pid ) -> gen_server:call( Pid, {get_value} ).
get_active( Pid ) -> gen_server:call( Pid, {get_active} ).
get_name( Pid ) -> gen_server:call( Pid, {get_name} ).
get_description( Pid ) -> gen_server:call( Pid, {get_description} ).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { parent, name, value = 0, active = true, previous, description = <<"">> }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Parent, Name, Options]) ->
	
    {ok, #state{ 	parent = Parent, 
    				name = Name,
    				value = 0,
    				active = proplists:get_value( Options, active, true ) ,
    				description = proplists:get_value( Options, description, <<"">> )
    		}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

handle_call( {stop, Reason}, _From, State ) ->
	{stop, Reason, ok, State};

handle_call( {get_active}, _From, State ) ->
	{reply, {ok, State#state.active}, State };

handle_call( {get_description}, _From, State ) ->
	{reply, {ok, State#state.description}, State };

handle_call( {get_name}, _From, State ) ->
	{reply, {ok, State#state.name}, State };

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

