%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 17 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------

%% see inet:setopts/2 for valid options
%% if not set, the following values are used:
%% 	      {tcp_opts, [{keepalive, true},
%% 			  {nodelay, true},
%% 			  {tos, 16#14}]},  % MINDELAY and RELIABILITY
%% see man page ip(7) -> netinet/ip.h for TOS values

-define(TCP_OPTS, [binary, {packet,raw},
                   %% {keepalive,true}, {nodelay,true},
                   {reuseaddr,true}, {active,false}]).

-define(TCP_TIMEOUT, 60000).

