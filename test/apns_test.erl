-module(apns_test).

-include("apns.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").

-define(ID, 'apns_test').

%% write your device token into the corresponding '.token' file
%% the device token is 64 chars long.
-define(TOKEN_FILE, "priv/test.token").
-define(TOKEN_SANDBOX_FILE, "priv/test.sandbox.token").
%% or
%% replace the string of token with your own.
-define(TOKEN, "0000000000000000000000000000000000000000000000000000000000000000").
%% @hidden
get_token() ->
	File=	case apns:get_env(sandbox, false) of
				true -> ?TOKEN_SANDBOX_FILE;
				false-> ?TOKEN_FILE
			end,
	case filelib:is_file(File) of
		true ->
			{ok, <<Token:64/binary,_/binary>>}= file:read_file(File),
			erlang:display("Using token loaded from file: "++File),
			erlang:display(binary_to_list(Token)),
			Token;
		false->
			erlang:display("Using token defined by macro."),
			erlang:display(?TOKEN),
			?TOKEN
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec apns_test_() -> {setup, fun(() -> ok), fun((_) -> ok), {timeout, 60, fun(() -> any())}}.
apns_test_() ->
	{setup,
		fun() ->
				erlang:display("setup ..."),
				?assertMatch({ok,_}, application:ensure_all_started(apns)),
				erlang:display("connecting ..."),
				%%?assertMatch({ok,_}= apns:connect(?ID)),
				?assertMatch({ok,_}, apns:connect(?ID, fun fun_error/2, fun fun_feedback/1)),
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
	?assertEqual(ok, apns:send_message(?ID, get_token(), list_to_binary(Str))).


%% @hidden
fun_error(MsgId, Status) ->
	error_logger:error_msg("APNS test fun_error: #~p, ~p",
						   [apns:message_id_to_integer(MsgId), Status]).
%% @hidden
fun_feedback(Any) ->
	error_logger:warning_msg("APNS test fun_feedback: ~p.", [Any]).


-spec test() -> any().
