-module( ct_cs_test ).

-compile( [export_all] ).


-define( COUNTERS, [
						{ <<"DomainA">>, <<"counter10">>, <<"">> },
						{ <<"DomainA">>, <<"counter12">>, <<"">> },
						{ <<"DomainB">>, <<"counter21">>, <<"">> },
						{ <<"DomainC">>, <<"counter31">>, <<"">> },
						{ <<"DomainC">>, <<"counter32">>, <<"">> }
					]).

ct_register_unregister_domain() ->
	
	application:start( sasl ),

	io:format("Starting counter_service ~n" ),

	case counter_service:start() of
		{ok, Pid} ->
			io:format("Done ~p ~n", [Pid] ),

			lists:foreach( 	fun( {Dom, Cnt, Desc} ) -> 
								io:format("Registering: ~p ~p ~p ", [Dom, Cnt, Desc]),
								case counter_service:register_counter( Dom, Cnt, [{description, Desc}] ) of
									{ok, Pid} -> io:format(" done ~p ~n", [Pid]); 
									Other -> io:format(" failed ~p ~n", [Other])
								end
							end, 
							?COUNTERS 
						),

			ok;
		Other ->
			io:format("Failed : ~p ~n", [Other] ),

			ok
	end,

	counter_service:stop(),
	application:stop( sasl ).