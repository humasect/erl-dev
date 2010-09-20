%%%-------------------------------------------------------------------
%%% @author Lyndon Tremblay <humasect@gmail.com>
%%% @copyright (C) 2010, Lyndon Tremblay
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2010 by Lyndon Tremblay <humasect@gmail.com>
%%%-------------------------------------------------------------------
-module(zen_web).
-author('humasect@gmail.com').

-export([start/0, restart/0, stop/0, config/1]).

-define(AUTH_PASS, "valhalla").

%%%===================================================================
%%% API
%%%===================================================================

-define(webconf(X), ?MODULE:config(X)).

config({auth_dir,Name}) ->
    {Realm,Groups} =
        case Name of
            "game" -> {"Gamelike", ["dev", "gm", "player"]};
            "stats" -> {"Gamelike Stats", ["dev", "gm", "vendor"]};
            _ -> {"Valhalla", ["dev"]}
        end,

    {?webconf(root) ++ "/" ++ Name ++ "/",
     [{auth_name,Realm},
      {auth_type,mnesia},
      {auth_access_password, ?AUTH_PASS},
      {require_group, Groups}]}
        ;
config(root) ->
    code:priv_dir(zen) ++ "/www"
        ;
config(port) ->
    {ok,Port} = application:get_env(zen, web_port),
    Port
        ;
config(inets) ->
    {ok,Hostname} = inet:gethostname(),

    [{server_root, code:priv_dir(zen) ++ "/log"},
     {document_root, ?webconf(root)},
     {server_name, Hostname},
     {port, ?webconf(port)},
     {bind_address, any},

     {error_log, "error.log"},
     {security_log, "security.log"},
     {transfer_log, "transfer.log"},

     {max_keep_alive_requests, 4},

     %%{security_directory, {Priv++"/www/game",
     %%                      [{security_data_file, "security.dat"}]}},

     {directory_index, ["index.html"]},
     {server_admin, "humasect@gmail.com"},

     {directory, ?webconf({auth_dir, "game"})},
     {directory, ?webconf({auth_dir, "stats"})},

     %% {directory, {Dir ++ "/game/", [{auth_name, "Gamelike"},
     %%                                {auth_type, mnesia},
     %%                                {auth_access_password, ?AUTH_PASS},
     %%                               {require_group, ["dev","gm","player"]}]}},
     {modules, [zen_mod_websocket,
                mod_alias,
                %%mod_security,
                mod_auth,
                mod_esi, mod_actions, mod_cgi,
                mod_dir, mod_get, mod_head, mod_log, mod_disk_log,
                mod_htaccess, mod_include]}]
        ;
config({pid_at,DirName}) ->
    [{dir, ?webconf(root) ++ "/"++DirName++"/"},
     {port, ?webconf(port)},
     {authPassword,?AUTH_PASS}
    ].

start() -> 
    inets:start(httpd, ?webconf(inets)).

restart() ->
    httpd:reload_config(?webconf(inets), non_disturbing).

stop() ->
    Pid = proplists:get_value(httpd, inets:services()),
    inets:stop(httpd, Pid).
