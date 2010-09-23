%%% -*- erlang -*- 

{application, val,
 [{description, "Valhalla Server"},
  {vsn, "0.2"},
  {modules, [val_app, val_sup]},
  {registered, []},
  {applications, [kernel, stdlib, zen]}
 ]}.


