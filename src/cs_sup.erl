-module( cs_sup ).

-behaviour( supervisor ).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ start_link/1 ]).

start_link( Args ) ->
	supervisor:start_link( {local, ?MODULE}, ?MODULE, [] ).

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

    Service = {'cs_service',{'cs_service',start_link,[]},
      permanent,2000, worker, ['cs_service'] },

    DomainSup = {'cs_domain_sup',{'cs_domain_sup',start_link,[]},
      permanent,2000, supervisor, ['cs_domain_sup'] },

    CounterSup = {'cs_counter_sup',{'cs_counter_sup',start_link,[]},
      permanent,2000, supervisor, ['cs_counter_sup'] },

	Children = [Service, DomainSup, CounterSup],
    {ok,{{one_for_all,0,1}, Children }}.

%% ====================================================================
%% Internal functions
%% ====================================================================


