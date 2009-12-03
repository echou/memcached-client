% vim:syn=erlang
{application, mcache,
 [{description, "memcached client application"},
  {vsn, "1.0.0"},
  {modules, [%MODULES%]},
  {registered, []},
  {mod, {mcache_app, []}},
  {env, []},
  {applications, [kernel,stdlib]}
 ]
}.

