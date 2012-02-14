{application,ecache_app,
             [{description,"Cache Library"},
              {vsn,"0.0.1"},
              {id,"ecache_app"},
              {modules,[ecache_app,ecache_server,ecache_sup]},
              {registered,[]},
              {env,[]},
              {mod,{ecache_app,[{rec_max_size,1000000000},{proc_num,4}]}},
              {applications,[kernel,stdlib]}]}.
