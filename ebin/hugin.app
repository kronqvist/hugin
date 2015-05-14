{application, hugin,
  [{description, "A Framework for Website Crawling"},
   {vsn, "1.0.0"},
   {registered, []},
   {applications, [kernel, stdlib, hackney]},
   {mod, {hugin_app, []}},
   {env, []},
   {modules, [hugin, raven, raven_cfg, hugin_worker, hugin_sup,
              hugin_server_sup, hugin_app, hugin_server]}]}.
