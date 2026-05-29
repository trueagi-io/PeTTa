;Configured at runtime memory and history in amount of characters:
(= (maxFeedback) (empty))
(= (maxRecallItems) (empty))
(= (maxEpisodeRecallLines) (empty))
(= (maxHistory) (empty))
(= (embeddingprovider) (empty))

(= (initMemory)
   (progn (println! "Initializing memory")
          (configure maxFeedback 50000)
          (configure maxRecallItems 20)
          (configure maxEpisodeRecallLines 20)
          (configure maxHistory 30000)
          (configure embeddingprovider Local) ;OpenAI or Local
          (if (== (embeddingprovider) Local)
              () _)))

(= (getPrompt)
   (read-file (library OmegaClaw-Core ./memory/prompt.txt)))

(= (getHistory)
   (read_file_tail (library OmegaClaw-Core ./memory/history.metta) (maxHistory)))

(= (addToHistory $lastmessage $response $sexpr $msgnew)
   (if $msgnew
       (if (== (get-state &error) ())
           (appendToHistory ((get_time_as_string) (newline) "HUMAN_MESSAGE: " $lastmessage (newline) $response (newline)))
           (appendToHistory ((get_time_as_string) (newline) "HUMAN_MESSAGE: " $lastmessage (newline) $response (newline) ERROR_FEEDBACK: (get-state &error))))
       (if (== (get-state &error) ())
           (appendToHistory ((get_time_as_string) (newline) $response (newline)))
           (appendToHistory ((get_time_as_string) (newline) $response (newline) ERROR_FEEDBACK: (get-state &error))))))

(= (embed $str)
   (if (== (embeddingprovider) Local)
       (call-embedding (string-safe $str) (embeddingprovider))
       (useGPTEmbedding (string-safe $str))))

(= (appendToHistory $addition)
   (append-file (library OmegaClaw-Core ./memory/history.metta) (swrite $addition)))

(= (remember $str)
   (progn (vector-remember $str (embed $str) (get_time_as_string))
          REMEMBER-SUCCESS))

(= (query $str)
   (vector-query (embed $str) (maxRecallItems)))

(= (episodes $time)
   (around-time $time (maxEpisodeRecallLines)))
