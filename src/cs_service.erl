-module( cs_service ).
-behaviour(gen_server).


-include( "../include/counter_service.hrl" ).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link/1, stop/0, stop/1]).
-export([
		register_domain/1, register_domain/2,
		register_counter/2, register_counter/3,
		unregister_domain/1,
		unregister_counter/2
	]).

-define( SERVER, ?SERVER_SERVCE ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% domain_list :: [domainitem()|list()]
%%		domainitem() :: {Name :: atom(), Pid :: pid() }
-record(state, { domain_list = [] }).

%% start_link/0
%% ====================================================================
%% ====================================================================
start_link() ->
	start_link( [] ).

%% start_link/1
%% ====================================================================
%% ====================================================================
start_link( Opt ) ->
	gen_server:start_link( {local, ?SERVER}, ?MODULE, [Opt], [] ).

%% stop/0
%% ====================================================================
%% ====================================================================
stop() ->
	stop( normal ).

%% stop/1
%% ====================================================================
%% ====================================================================
stop( Reason ) ->
	gen_server:call( ?SERVER, { stop, Reason } ).


%% register_domain/1
%% ====================================================================
-spec register_domain( Domain ) -> {error, Reason} | {ok, Pid } when 
	Domain :: binary(),
	Reason :: atom(),
	Pid :: pid().
%% ====================================================================
register_domain( Domain ) ->
	register_domain( Domain, [] ).

%% register_domain/2
%% ====================================================================
-spec register_domain( Domain, Options ) -> {error, Reason} | {ok, Pid } when
	Domain :: binary(),
	Options :: list(),
	Reason :: atom(),
	Pid :: pid().
%% ====================================================================
register_domain( Domain, Options ) when is_atom( Domain ) ->
	register_domain( atom_to_binary( Domain, latin1 ), Options );

register_domain( Domain, Options ) when is_list( Domain ) ->
	register_domain( list_to_binary( Domain ), Options );

register_domain( Domain, Options ) ->
	gen_server:call(  ?SERVER, {register_domain, Domain, Options} ).

%% unregister_domain/1
%% ====================================================================
%% ====================================================================
unregister_domain( Domain ) when is_atom( Domain ) ->
	unregister_domain( atom_to_binary( Domain, latin1 ) );

unregister_domain( Domain ) when is_list( Domain ) ->
	unregister_domain( list_to_binary( Domain ) );

unregister_domain( Domain ) ->
	gen_server:cast( ?SERVER, {unregister_domain, Domain } ).

%% register_counter/2
%% ====================================================================
%% ====================================================================
register_counter( Domain, Counter ) ->
	register_counter( Domain, Counter, [] ).

%% register_counter/3
%% ====================================================================
%% ====================================================================
register_counter( Domain, Counter, Options ) when is_atom( Domain ) ->
	register_counter( atom_to_binary( Domain, latin1) , Counter, Options );

register_counter( Domain, Counter, Options ) when is_list( Domain ) ->
	register_counter( list_to_binary( Domain ), Counter, Options );

register_counter( Domain, Counter, Options ) ->
	gen_server:call( ?SERVER, {register_counter, Domain, Counter, Options} ).

%% unregister_counter/2
%% ====================================================================
%% ====================================================================
unregister_counter( Domain, Counter ) ->
	gen_server:cast( ?SERVER, {unregister_counter, Domain, Counter } ).



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
init( Options ) ->
	erlang:process_flag( trap_exit, true ),

    {ok, #state{ domain_list = [] } }.


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
handle_call( {register_domain, Domain, Options}, _From, #state{ domain_list = DomainList } = State ) ->
	case x_register_domain( Domain, Options, DomainList ) of
		{ok,Pid} ->
			{reply, {ok, Pid}, State#state{ 
				domain_list = [{Domain, Pid}|DomainList] 
			}};
	{error, Reason} -> 
			{reply, {error, Reason}, State}
	end;

handle_call( {unregister_domain, Domain}, _From, #state{ domain_list = DomainList } = State ) ->
	case x_unregister_domain( DomainList, DomainList ) of
		{error, Reason} -> {reply, {error, Reason}, State };
		List -> 
			{reply, ok, State#state{ domain_list = List} }
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
handle_cast( {unregister_domain, Domain}, #state{ domain_list = DomainList } = State ) ->
	case x_unregister_domain( DomainList, DomainList ) of
		{error, Reason} -> {reply, {error, Reason}, State };
		List -> 
			{reply, ok, State#state{ domain_list = List} }
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
	case x_terminate_domain( State#state.domain_list ) of
		{error, Reason} -> error_logger:error_msg( "[~p] ERROR: Could not terminate domain list: ~p~n", [?MODULE, Reason]), ok;
		N -> ok
	end.


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


%% x_terminate_domain/1
%% ====================================================================
%% Terminate list of domains, used to terminate all registered domains in service
-spec x_terminate_domain( DomainList ) -> N when
	DomainList :: list(), 
	N :: non_neg_integer().
%% ====================================================================
x_terminate_domain( DomainList ) ->
	x_terminate_domain( DomainList, 0).

x_terminate_domain( [], N ) ->	N;

x_terminate_domain( [{K,DomPid}|List], N) ->
	io:format(">>>>>>>>>>>>>> ~p ~p ~n", [K, DomPid] ),
	case ?MOD_DOMAIN:stop( DomPid ) of
		{error, Reason} -> 
			error_logger:error_msg("[~p] ERROR: Could not terminate domain ~p : ~p ~n", [?MODULE, K,Reason]),
			x_terminate_domain( List, N + 1);
		ok -> 
			x_terminate_domain( List, N + 1)
	end.




%% x_register_domain/2
%% ====================================================================
%% Terminate list of domains, used to terminate all registered domains in service
-spec x_register_domain( Domain, Options, DomainList ) -> {ok, Pid} | {error, Reason} when
	Domain :: binary(),
	Options :: list(),
	DomainList :: list(),
	Pid :: pid(),
	Reason :: atom().
%% ====================================================================
x_register_domain( Domain, Options, DomainList ) ->
	case x_domain_exists( Domain, DomainList ) of
		false ->
			case x_start_domain( Domain, Options ) of
				{ok, Pid} -> {ok,Pid};	
				{error, Reason} ->
					error_logger:error_msg("[~p] ERROR: Could not start domain ~p : ~p", [?MODULE, Domain, Reason] ),
					{error, Reason}
			end;
		{error, Reason} ->
			error_logger:error_msg("[~p] ERROR: Could not search domain ~p : ~p", [?MODULE, Domain, Reason] ),
			{error, Reason};
		Pid when is_pid( Pid ) -> {ok, Pid};
		Other -> Other
	end.


%% x_unregister_domain/2
%% ====================================================================
-spec x_unregister_domain( Domain, DomainList ) -> NewList | {error, Reason} when
	Domain :: binary(),
	DomainList :: list(),
	NewList :: list(),
	Reason :: missing | tuple().
%% ====================================================================
x_unregister_domain( Domain, DomainList ) ->
	case x_domain_exists( Domain, DomainList ) of
		false -> 
			error_logger:error_msg("[~p] ERROR: Removing Non Existing domain ~p",[?MODULE, Domain]),
			{error, missing};
		Pid when is_pid( Pid ) ->
			try ?MOD_DOMAIN:unregister_domain( Pid ) of
				ok -> lists:keydelete( Domain, 1, DomainList );
				{error, Reason} -> {error, Reason}
			catch
				Error:Reason ->
					{error, Reason}
			end;
		Other -> {error, Other}
	end.

%%
%% ====================================================================
%% ====================================================================
x_register_counter( Domain, Counter, Options ) ->
	ok.

%%
%% ====================================================================
%% ====================================================================
x_unregister_counter( Domain, Counter ) ->
	ok.


%% x_start_domain/2
%% ====================================================================
%% Start a domain process 
-spec x_start_domain( Domain, Options ) -> {ok, Pid} | {error, Reason} when
	Domain :: binary(),
	Options :: list(),
	Pid :: pid(),
	Reason :: term().
%% ====================================================================
x_start_domain( Domain, Options ) ->
	case ?MOD_DOMAIN:register_domain( self(), Domain, Options ) of
		{ok, Pid} -> {ok, Pid};
		{error, Reason} -> {error, Reason}
	end.

%% x_domain_exists/2
%% ====================================================================
%% Checks if the domain is already registered.
%% If Domain exists Pid is returned.
%% If Domain is missing, false isreturned.
-spec x_domain_exists( SearchDomain, List ) -> Pid | false when
	SearchDomain :: binary(),
	List :: list(),
	Pid :: pid().
%% ====================================================================
x_domain_exists( SearchDomain, List ) ->
	case lists:keyfind( SearchDomain, 1, List ) of
		false -> false;
		{SearchDomain, Pid} -> Pid
	end.

