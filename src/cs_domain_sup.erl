-module( cs_domain_sup ).
 
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/0, start_domain/2, start_domain/3]).


start_link( ) ->
	supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).

start_domain( Parent, Name ) ->
	start_domain( Parent, Name, [] ).

start_domain( Parent, Name, Options ) ->
	supervisor:start_child( Parent, Name ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    AChild = {'cs_domain',{'cs_domain',start_link,[]},
	      temporary,2000,worker,['cs_domain']},
    {ok,{{one_for_all,0,1}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


