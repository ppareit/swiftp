{application, swiftp_proxy,
  [
    {description,    "Multi-user FTP proxy server for use with SwiFTP Android app"},
   {id,             ""},
   {vsn,            "0.1"},
   {modules,        [client_session, client_session_sup, db, device_session, 
                     device_session_sup, log_gen_event, log, rand, session_registry,
                     swiftp_proxy, tcp_listener, util]},
   {registered,     logger},
   {mod,            {swiftp_proxy, []}}
  ]
}.
   
