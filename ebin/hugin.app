{application, hugin,
  [{description, "A Framework for Website Crawling"},
   {vsn, "1.0.0"},
   {registered, []},
   {applications, [kernel, stdlib, hackney]},
   {mod, {hugin_app, []}},
   {env, []},
   {modules, [hugin, raven, hugin_opts, hugin_worker, hugin_sup,
              hugin_server_sup, hugin_html, hugin_app, hugin_server]}]}.
