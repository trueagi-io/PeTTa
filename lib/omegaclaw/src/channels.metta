;configured at runtime:
(= (IRC_channel) (empty))
(= (IRC_server) (empty))
(= (IRC_port) (empty))
(= (IRC_user) (empty))
(= (commchannel) (empty))
(= (MM_URL) (empty))
(= (MM_CHANNEL_ID) (empty))
(= (MM_BOT_TOKEN) (empty))
(= (TG_BOT_TOKEN) (empty))
(= (TG_CHAT_ID) (empty))
(= (TG_POLL_TIMEOUT) (empty))
(= (SL_BOT_TOKEN) (empty))
(= (SL_CHANNEL_ID) (empty))
(= (SL_POLL_INTERVAL) (empty))

;Connect all the channels:
(= (initChannels)
   (progn (println! "Initializing channels")
          (configure commchannel irc)
          (if (== (commchannel) irc)
              (progn (configure IRC_channel ##omegaclaw)
                     (configure IRC_server "irc.quakenet.org")
                     (configure IRC_port 6667)
                     (configure IRC_user omegaclaw)
                     (irc-connect (IRC_server) (IRC_port) (IRC_user) (IRC_channel) ""))
              (println! "Only IRC is natively supported in this version."))))

;Receive the latest user message considering all communication channels:
(= (receive)
   (if (== (commchannel) irc)
       (irc-recv)
       ""))

;Send a message to all communication channels:
!(change-state! &lastsend "")
(= (send $msg)
   (if (!= $msg (get-state &lastsend))
       (progn (change-state! &lastsend $msg)
              (let $safemsg (string-replace $msg  "\n" "\\n")
                   (if (== (commchannel) irc)
                       (let $temp (cut) (irc-send $safemsg))
                       (println! "Only IRC is natively supported in this version.")))) _))

;Search the internet for some information:
(= (search $msg)
   (web-search $msg))
