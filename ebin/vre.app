%%% -*- erlang -*- 

{application, vre,
 [{description, "VREnvironment Server"},
  {vsn, "0.3"},
  {modules, [vre_app, vre_sup]},
  {registered, []},
  {applications, [kernel, stdlib, zen]}
 ]}.


  
