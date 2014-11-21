%%-------------------------------------------------------------------
%% FILE: apns_counter.erl
%% 1/2014.11.18, BY 13592607645.
%% @author 13592607645 <13592607645@126.com>
%% @copyright (C) 2014 13592607645 <13592607645@126.com>
%% @doc A service for counting.
%% @end
%%-------------------------------------------------------------------

-module(apns_counter).
-behaviour(gen_server).
-author("13592607645").
-vsn(1).

-export([start_link/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([current/0, next/0, reset/0]).


%% @hidden
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @hidden
init(_) ->
	{ok, init_id()}.

%% @hidden
code_change(_,Last,_) ->
	{ok, Last}.

%% @hidden
terminate(Exit,_) ->
	Exit.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init_id() -> {Days::non_neg_integer(), Num::non_neg_integer()}.
init_id() ->
	{calendar:date_to_gregorian_days(date()),0}.

%% @hidden
-spec reset_id() -> {Days::non_neg_integer(), Num::non_neg_integer()}.
reset_id() ->
	{calendar:date_to_gregorian_days(date()),1}.

%% @hidden
-spec next_id({LastDays::non_neg_integer(), LastNum::non_neg_integer()}) -> {Days::non_neg_integer(), Num::non_neg_integer()}.
next_id({D,I}) when is_integer(D), is_integer(I) ->
	Days= calendar:date_to_gregorian_days(date()),
	{Days,	if
			   D=:=Days -> I+1;
			   true -> 1
			end
	};
next_id(_) ->
	reset_id().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
handle_call(current,_,Last) ->
	{reply, Last, Last};
handle_call(next,_,Last) ->
	Id= next_id(Last),
	{reply, Id, Id};
handle_call(reset,_,_) ->
	Id= reset_id(),
	{reply, Id, Id};
handle_call(stop,_,Last) ->
	{stop, stop, Last, Last}.

%% @hidden
handle_cast(stop,Last) ->
	{stop, stop, Last};
handle_cast(_,Last) ->
	{noreply, Last}.

%% @hidden
handle_info(timeout,Last) ->
	{noreply, Last};
handle_info(_,Last) ->
	{stop, stop, Last}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current() ->
	gen_server:call(?MODULE, current).

next() ->
	gen_server:call(?MODULE, next).

reset() ->
	gen_server:call(?MODULE, reset).

