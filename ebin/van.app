%%% -*- erlang -*-

{application, val,
 [{description, "Visionire Server"},
  {vsn, "1.0"},
  {modules, [
             van_app,
             van_sup,
             %%van_web_misultin,
             %%val_actor,
             %%val_map,
             %%val_party,
             %%val_client
            ]},
%  {registered, [val_login_server,val_actor_server,val_map_server]},
  {mod, {van_app,[]}},   % only needed if application needs to be started
  {env, [
         {web_port, 8000},
         {tcp_port, 8001},

         {irc_server, "velocity.ca.us.quakenet.org"},
         {irc_port, 6667},
         {irc_channels, ["#g.bt", "#rgrdbots"]},
         {irc_nick, "whomasect"}
        ]},

  {applications,[kernel,stdlib,mnesia,inets]}
 ]}.

