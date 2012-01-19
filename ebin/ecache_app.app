{application,ecache_app,
             [{description,"Cache Library"},
              {vsn,"0.0.1"},
              {id,"ecache_app"},
              {modules,[ecache_app,ecache_server,ecache_sup]},
              {registered,[]},
              {env,[]},
              {mod,{ecache_app,[{rec_max_size,1000000}]}},
              {applications,[kernel,stdlib]}]}.
