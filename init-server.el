;; Start the emacs server
(setq server-use-tcp t) ;; Use TCP mode, my socket is often unavailable
(setq server-host "127.0.0.1")
(if (functionp 'server-running-p)
    (if (not (server-running-p)) ;; Server might be running
        (server-start)))
