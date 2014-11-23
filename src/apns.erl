%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @doc Apple Push Notification Server for Erlang
%% @end
%%-------------------------------------------------------------------
-module(apns).
-vsn('1.1').
-date({2014,11,23}).

-include("apns.hrl").

-export([start/0, stop/0]).
-export([connect/1, disconnect/1, connect/3]).
-export([send_message/3]).

-type status() :: no_errors | processing_error | missing_token | missing_topic | missing_payload |
                  missing_token_size | missing_topic_size | missing_payload_size | invalid_token |
                  unknown.
-export_type([status/0]).

-type conn_id() :: atom() | pid().
-export_type([conn_id/0]).

-type apns_str() :: binary() | string().
-type alert() :: apns_str().
-export_type([alert/0]).


-define(EPOCH, 62167219200).
-define(MAX_PAYLOAD, 256).

-export([estimate_available_bytes/1]).
-export([message_id/0, expiry/1, timestamp/1]).


-export([get_env/2]).
-define(APNS_HOST, "gateway.push.apple.com").
-define(FEEDBACK_HOST, "feedback.push.apple.com").
-define(APNS_PORT, 2195).
-define(FEEDBACK_PORT, 2196).
-define(APNS_SANDBOX_HOST, "gateway.sandbox.push.apple.com").
-define(FEEDBACK_SANDBOX_HOST, "feedback.sandbox.push.apple.com").
-define(CERT_FILE, "priv/cert.pem").
-define(CERT_SANDBOX_FILE, "priv/cert.sandbox.pem").

-export([get_datetime/0]).
-export([expiry_to_datetime/1]).
-export([message_id_to_integer/1]).


%% @doc Starts the application
-spec start() -> ok | {error, {already_started, apns}}.
start() ->
	ssl:start(),
	application:start(apns).

%% @doc Stops the application
-spec stop() -> ok.
stop() ->
	application:stop(apns).


%% @doc Open an connection
-spec connect(ID::atom()) -> {ok, pid()} | {error, Reason::term()}.
connect(ID) ->
	apns_message_id:start_link(),
	Conn= make_connection(),
    error_logger:info_msg("Start connection(~p) with [~p] using cert of ~p.~n",
						 [ID,
						  Conn#apns_connection.apple_host,
						  Conn#apns_connection.cert_file]),
	apns_sup:start_connection(ID, Conn).

-spec connect(ID::atom(),
			  FunError::fun((MsgId::binary(),Status::atom())->any()),
			  FunFeedback::fun((term())->any())) ->
	{ok, pid()} | {error, Reason::term()}.
connect(ID, FunError, FunFeedback) ->
	apns_message_id:start_link(),
	Conn= make_connection(),
    error_logger:info_msg("Start connection/3(~p) with [~p] using cert of ~p.~n",
						 [ID,
						  Conn#apns_connection.apple_host,
						  Conn#apns_connection.cert_file]),
	apns_sup:start_connection(ID,
							  Conn#apns_connection{
								error_fun= FunError,
								feedback_fun=FunFeedback
							   }).


%% @doc Close the connection
-spec disconnect(ID::atom()) -> ok.
disconnect(ID) ->
	apns_connection:stop(ID).

%% @doc Sends a message to Apple
-spec send_message(ID::atom(), Token::apns_str(), Text::apns_str()) -> ok.
send_message(ID, Token, Text) ->
	Msg= #apns_msg {
		badge= 0,
		sound= "default",
		device_token=
			if
				is_list(Token)  -> Token;
				is_binary(Token)-> binary_to_list(Token)
			end,
		alert= Text
	},
	apns_connection:send_message(ID, Msg).


%% @doc Predicts the number of bytes left in a message for additional data.
-spec estimate_available_bytes(#apns_msg{}) -> integer().
estimate_available_bytes(#apns_msg{} = Msg) ->
  Payload = apns_connection:build_payload(Msg),
  ?MAX_PAYLOAD - erlang:size(list_to_binary(Payload)).

%% @doc  Generates sequential message Id
-spec message_id() -> binary().
message_id() ->
	{_Days, Num}= apns_message_id:next(),
	<<Num:4/unsigned-integer-unit:8>>.

%% @doc  Generates a valid expiry value for messages.
%%       If called with <code>none</code> as the parameter, it will return a <a>no-expire</a> value.
%%       If called with a datetime as the parameter, it will convert it to a valid expiry value.
%%       If called with an integer, it will add that many seconds to current time and return a valid
%%        expiry value for that date.
-spec expiry(none | {{1970..9999,1..12,1..31},{0..24,0..60,0..60}} | pos_integer()) -> non_neg_integer().
expiry(none) -> 0;
expiry(Secs) when is_integer(Secs) ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - ?EPOCH + Secs;
expiry(Date) ->
  calendar:datetime_to_gregorian_seconds(Date) - ?EPOCH.

-spec timestamp(pos_integer()) -> calendar:datetime().
timestamp(Secs) ->
  calendar:gregorian_seconds_to_datetime(Secs + ?EPOCH).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_env(Key::atom(), Def::term()) -> Val::term().
get_env(Key, Def) ->
	case application:get_env(apns, Key, Def) of
		default-> Def;
		Other  -> Other
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec make_connection() -> #apns_connection{}.
make_connection() ->
	Conn=	case get_env(sandbox, false) of
				true -> #apns_connection {
						cert_file= ?CERT_SANDBOX_FILE,
						apple_host= ?APNS_SANDBOX_HOST,
						feedback_host= ?FEEDBACK_SANDBOX_HOST
					};
				false-> #apns_connection {
						cert_file= ?CERT_FILE,
						apple_host= ?APNS_HOST,
						feedback_host= ?FEEDBACK_HOST
					}
			end,
	Conn#apns_connection {
		apple_port= ?APNS_PORT,
		feedback_port= ?FEEDBACK_PORT,
        cert_file=get_env(cert_file, Conn#apns_connection.cert_file),
        key_file= get_env(key_file, Conn#apns_connection.key_file),
        cert_password= get_env(cert_password, Conn#apns_connection.cert_password),
        timeout= get_env(timeout, Conn#apns_connection.timeout),
        feedback_timeout= get_env(feedback_timeout, Conn#apns_connection.feedback_timeout),
		error_fun=	fun(MsgId, Status) ->
						error_logger:error_msg("APNS error: #~p, ~p",
											   [message_id_to_integer(MsgId), Status])
					end,
		feedback_fun= fun(Any) ->
						error_logger:warning_msg("APNS feedback: ~p.", [Any])
					end
	}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec format_datetime(calendar:datetime()) -> string().
format_datetime(Datetime) ->
	{{Year,Month,Day},{Hour,Minute,Second}}= Datetime,
	lists:flatten(
		io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
			[Year,Month,Day,Hour,Minute,Second])).

%% @doc  Get local string of the local time.
-spec get_datetime() -> string().
get_datetime() ->
	format_datetime(calendar:local_time()).

%% @doc  Convert the expiry value to local string of the local time.
-spec expiry_to_datetime(Sec::non_neg_integer()) -> string().
expiry_to_datetime(Sec) ->
	format_datetime(
		calendar:universal_time_to_local_time(
			calendar:gregorian_seconds_to_datetime(Sec+?EPOCH))).

%% @doc  Convert the sequential message Id to integer value.
-spec message_id_to_integer(Id::binary()) -> integer() | -1.
message_id_to_integer(<<Id:4/unsigned-integer-unit:8,_/binary>>) ->
	Id.

