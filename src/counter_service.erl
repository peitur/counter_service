-module( counter_service ).

-include( "../include/counter_service.hrl" ).

-export([
		start/0,
		stop/0
	]).

-export([
		register_domain/1, register_domain/2,
		unregister_domain/1,
		register_counter/2, register_counter/3,
		unregister_counter/2
	]).


start( ) ->
	application:start( ?MODULE ).

stop( ) ->
	application:stop( ?MODULE ).



register_domain( Domain ) when is_list( Domain ) ->
	register_domain( list_to_binary( Domain ) );

register_domain( Domain ) when is_atom( Domain ) ->
	register_domain( atom_to_binary( Domain, latin1 ) );

register_domain( Domain ) ->
	?MOD_SERVICE:register_domain( Domain ).

register_domain( Domain, Options ) ->
	?MOD_SERVICE:register_domain( Domain, Options ).

unregister_domain( Domain ) ->
	?MOD_SERVICE:unregister_domain( Domain ).

register_counter( Domain, Counter ) ->
	?MOD_SERVICE:register_counter( Domain, Counter ).

register_counter( Domain, Counter, Options ) ->
	?MOD_SERVICE:register_counter( Domain, Counter, Options ).

unregister_counter( Domain, Counter ) ->
	?MOD_SERVICE:unregister_counter( Domain, Counter ).



