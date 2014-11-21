-module(apns_test).

-include("apns.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
%% replace the token(64 chars) with your own!
-define(TOKEN, "0000000000000000000000000000000000000000000000000000000000000000").

-define(ID, 'apns_test').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec apns_test_() -> {setup, fun(() -> ok), fun((_) -> ok), {timeout, 60, fun(() -> any())}}.
apns_test_() ->
	{setup,
		fun() ->
				erlang:display("setup ..."),
				{ok,_}= application:ensure_all_started(apns),
				erlang:display("connecting ..."),
				{ok,_}= apns:connect(?ID),
				erlang:display("connected!")
			end,
		fun(_)->
				?assertEqual(ok, apns:disconnect(?ID)),
				?assertEqual(ok, apns:stop())
			end,
		{timeout, 60, fun run/0}
	}.

%%% Tests
-spec run() -> any().
run() ->
	Ref = erlang:monitor(process, ?ID),

	loop(1, fun()-> spawn(fun run_once/0) end),

	receive
		{'DOWN', Ref, _, _, _} = Msg ->
			?fail(Msg);
		Msg ->
			?fail(Msg)
		after 2000 ->
			ok
	end.

-spec loop(pos_integer(), fun(()->any())) -> any().
loop(Cnt,Act) when Cnt>0 ->
	Act(),
	loop(Cnt-1,Act);
loop(_,_) ->
	ok.

-spec run_once() -> any().
run_once() ->
	Str= lists:flatten(
		   io_lib:format(" [~p] 你好！测试。",
						 [apns:get_datetime()])),
	?debugVal(Str),
	?assertEqual(ok, apns:send_message(?ID, ?TOKEN, list_to_binary(Str))).

-spec test() -> any().
