{application,impactor,
             [{description,"WS Comet Connection Tester"},
              {vsn,"1.0.0"},
              {registered,[]},
              {applications,[kernel,stdlib,lager,mochiweb]},
              {mod,{impactor_app,[]}},
              {env,[]},
              {modules,[client,comet,impactor,impactor_app,impactor_sup,
                        protocol_util,sup_util,websocket_client]}]}.
