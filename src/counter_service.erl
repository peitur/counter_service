-module( counter_service ).

-export([
		start/0,
		stop/0
	]).




start( ) ->
	application:start( ?MODULE ).

stop( ) ->
	application:stop( ?MODULE ).




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



