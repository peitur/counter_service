-module( ct_cs_test ).

-compile( [export_all] ).


-define( COUNTERS, [
%						{ <<"DomainA">>, <<"counter10">>, <<"description 1">> },
%						{ <<"DomainA">>, <<"counter12">>, <<"description 2">> },
%						{ <<"DomainB">>, <<"counter21">>, <<"description 3">> },
%						{ <<"DomainC">>, <<"counter31">>, <<"description 4">> },
						{ <<"DomainC">>, <<"counter32">>, <<"description 5">> }
					]).

ct_register_unregister_domain() ->
	
	application:start( sasl ),
	application:start( counter_service ),

	io:format("Starting counter_service ~n" ),

	lists:foreach( 	fun( {Dom, Cnt, Desc} ) ->

						io:format("Registering: ~p ~p ~p ", [Dom, Cnt, Desc]),
						case counter_service:register_domain( Dom ) of
							{error, Reason} -> 
								 io:format(" domain failed ~p ~n", [Reason]);

							{ok, DomPid} ->
								case counter_service:register_counter( Dom, Cnt, [{description, Desc}] ) of
									{ok, Pid} -> io:format(" done ~p ~n", [Pid]); 
									Other -> io:format(" failed ~p ~n", [Other])
								end

						end
					end,
					?COUNTERS 
				),


	counter_service:stop(),
	application:stop( sasl ).