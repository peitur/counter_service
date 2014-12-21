-module( cs_service ).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link/1, stop/0, stop/1]).
-export([

	]).

-define( SERVER, counter_service ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { domain_list = [] }).


start_link() ->
	start_link( [] ).

start_link( Opt ) ->
	gen_server:start_link( {local, ?SERVER}, ?MODULE, [Opt], [] ).

stop() ->
	stop( normal ).

stop( Reason ) ->
	gen_server:call( ?SERVER, { stop, Reason } ).


register_domain( Domain ) ->
	cs_service:register_domain( Domain ).

register_domain( Domain, Options ) ->
	cs_service:register_domain( Domain, Options ).

unregister_domain( Domain ) ->
	cs_service:unregister_domain( Domain ).

register_counter( Domain, Counter ) ->
	cs_service:register_counter( Domain, Counter ).

register_counter( Domain, Counter, Options ) ->
	cs_service:register_counter( Domain, Counter, Options ).

unregister_counter( Domain, Counter ) ->
	cs_service:unregister_counter( Domain, Counter ).



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
	erlang:process_flag( trap_exit, true ).	

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




x_terminate_domain( DomainList ) ->
	x_terminate_domain( DomainList, 0).

x_terminate_domain( [], N ) ->	N;

x_terminate_domain( [{K,DomPid}|List], N) ->
	case cs_domain:stop( DomPid ) of
		{error, Reason} -> 
			error_logger:error_msg("[~p] ERROR: Could not terminate domain ~p : ~p ~n", [?MODULE, K,Reason]),
			x_terminate_domain( List, N + 1);
		ok -> 
			x_terminate_domain( List, N + 1)
	end.






