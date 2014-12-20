-module( cs_domain ).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		register_domain/2,register_domain/3,
		unregister_domain/1, unregister_domain/2,
		start_domain/2,start_domain/3,
		start_link/2, start_link/3,
		stop/1,stop/2
	]).

-export([
		tick_counter/2, tick_counter/3,
		tick_counter_aync/2, tick_counter_aync/3
	]).

-export([
		get_counter/1, get_counter/2,
		get_name/1,
		get_description/1
	]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%% counter_list = [ { counter_name:strin(), counter_pid:pid() } ]
-record(state, { parent, name, description = <<"">>, counter_list = [] }).

register_domain( Parent, Name ) ->
	start_domain( Parent, Name ).

register_domain( Parent, Name, Options ) ->
	start_domain( Parent, Name, Options ).

unregister_domain( Pid ) ->
	stop( Pid ).

unregister_domain( Pid, Reason ) ->
	stop( Pid, Reason ).

start_domain( Parent, Name ) ->
	start_domain( Parent, Name, [] ).

start_domain( Parent, Name, Options ) ->
	cs_domain_sup:start_domain( ?MODULE, [Parent, Name, Options], [] ).

start_link( Parent, Name ) ->
	start_link( Parent, Name, [] ).

start_link( Parent, Name, Options ) ->
	gen_server:start_link( ?MODULE, [Parent, Name, Options], [] ).

register_counter( Pid, Name ) ->
	register_counter( Pid, Name, [] ).

register_counter( Pid, Name, Options ) ->
	gen_server:call( Pid, {start_counter, Name, Options} ).

unregister_counter( Pid, Name ) ->
	register_counter( Pid, Name, normal ).

unregister_counter( Pid, Name, Reason ) ->
	gen_server:call( Pid, {stop_counter, Name, Reason} ).

stop( Pid ) ->
	stop( Pid, normal ).

stop( Pid, Reason ) ->
	gen_server:call( Pid, {stop, Reason} ).

tick_counter( DomainPid, CounterName ) ->
	tick_counter( DomainPid, CounterName, 1 ).

tick_counter( DomainPid, CounterName, Val ) ->
	gen_server:call( DomainPid, {tick_counter, CounterName, Val} ).

tick_counter_aync( DomainPid, CounterName ) ->
	tick_counter_aync( DomainPid, CounterName, 1 ).

tick_counter_aync( DomainPid, CounterName, Val ) ->
	gen_server:cast( DomainPid, {tick_counter, CounterName, Val} ).

get_counter( DomainPid ) ->
	get_counter( DomainPid, [] ).

get_counter( DomainPid, CounterList ) ->
	gen_server:call( DomainPid, {get_counter, CounterList} ).

get_name( DomainPid ) -> gen_server:call( DomainPid, {get_name} ).
get_description( DomainPid ) -> gen_server:call( DomainPid, {get_description} ).


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
	erlang:process_flag( trap_exit, true ),

    {ok, #state{ 	parent = Parent, 
    				name = Name, 
    				description = <<"">> 
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
handle_call( {get_counter, ReqList}, _From, #state{ counter_list = CounterList} = State ) ->
	{reply, ok, State};

handle_call( {get_description}, _From, State ) ->
	{reply, {ok, State#state.description}, State };

handle_call( {get_name}, _From, State ) ->
	{reply, {ok, State#state.name}, State };

handle_call( {tick_counter, CounterName, Val}, _From, #state{ counter_list = CounterList } = State ) ->
	case lists:keyfind( CounterName, 1, CounterList ) of
		{CounterName, Pid} ->
			case cs_counter:tick_counter( Pid, Val ) of
				{ok, Val} -> {reply, {ok, Val}, State};
				{error, Reason} -> 
					error_logger:error_msg("[~p] ERROR: Sync Could not iterate counter ~p: ~p ~n ", [?MODULE, CounterName, Reason] ),
					{reply, {error, Reason}, State};
				Other ->
				 	error_logger:error_msg("[~p] UNDEF: Sync Could not iterate counter ~p: ~p ~n ", [?MODULE, CounterName, Other] ),
					{reply, Other, State}				
			end;
		_Other ->
			{reply, {error, badcounter}, State}
	end;	


handle_call( {unregister_counter, Name, Reason}, _From, #state{ counter_list = CounterList } = State ) ->
	case lists:keyfind( Name, 1, CounterList ) of
		{Name, Pid} ->
			try cs_counter:unregister_counter( Pid, Name, Reason ) of
				{ok, Pid} ->

					{reply, ok, State#state{ counter_list = lists:keydelete(Name, 1, CounterList) } };
				{error, Reason} -> 
					error_logger:error_msg("[~p] ERROR: Could not unregister counter: ~p ~n", [?MODULE, Reason] ),
					{reply, {error, Reason}, State}
			catch
				Error:Reason ->
					error_logger:error_msg("[~p] ERROR: Could not unregister counter ~p: ~p ~n", [?MODULE, Name, Reason] ),
					{reply, {error, Reason}, State}
			end;
		_Other ->
			{reply, ok, State}
	end;


handle_call( {register_counter, Name, Options}, _From, #state{ counter_list = CounterList } = State ) ->
	case cs_counter:register_counter( self(), Name, Options ) of
		{ok, Pid} ->
			{reply, {ok, Pid}, State#state{ counter_list = [{Name, Pid}|CounterList] } };
		{error, Reason} -> 
			error_logger:error_msg("[~p] ERROR: Could not register counter: ~p ~n", [?MODULE, Reason] ),
			{reply, {error, Reason}, State}
	end;

handle_call( {stop, Reason}, _From, State ) ->
	{stop, Reason, ok, State};

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

handle_cast( {tick_counter, CounterName, Val}, #state{ counter_list = CounterList} = State ) ->
	case lists:keyfind( CounterName, 1, CounterList ) of
		{CounterName, Pid} ->
			case cs_counter:tick_counter( Pid, Val ) of
				{ok, Val} -> {noreply, State};
				{error, Reason} -> 
					error_logger:error_msg("[~p] ERROR: ASync Could not iterate counter ~p: ~p ~n ", [?MODULE, CounterName, Reason] ),
					{noreply, State};
				Other -> 
					error_logger:error_msg("[~p] UNDEF: ASync Could not iterate counter ~p: ~p ~n ", [?MODULE, CounterName, Other] ),
					{noreply, State}				
			end;
		_Other ->
			{reply, {error, badcounter}, State}
	end;	


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

handle_info( {'EXIT', FromPid, Reason}, State ) ->
	CounterList = State#state.counter_list,
	try lists:keydelete( FromPid, 2, CounterList ) of
		NewCounterList ->
			{noreply, State#state{ counter_list = NewCounterList } }
	catch
		Error:Reason ->
			error_logger:warning_msg("[~p] WARN: Terminated Counter not registerd in list :~p ~n", [?MODULE, Reason]), 
			{noreply, State}
	end;


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

