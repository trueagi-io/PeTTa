;Import Python helpers for OpenRouter
!(import! &self (library lib_llm.py))

;Create string by appending the list entries
(= (py-str-helper () $outp) $outp)
(= (py-str-helper $L $outp)
   (let* (($head (car-atom $L))
          ($tail (cdr-atom $L))
          ($outp2 (py-call (operator.add $outp (py-call (str $head))))))
     (py-str-helper $tail $outp2)))
(= (py-str $L) (py-str-helper $L ""))

;Evaluate expression via py-call:
(= (py-eval $str)
   (let $dict (py-call (dict))
        (py-call (quote (eval $str $dict $dict)))))

;Add to builtins a function that can call functions via keyword arguments
!(py-eval "setattr(__import__('builtins'),'apply_kwargs',(lambda f,d: f(**d))) or 0")
;Add to builtins a function that can access by index:
!(py-eval "setattr(__import__('builtins'),'index', (lambda o,i: o[i])) or 0")
;Dict helpers:
!(py-eval "setattr(__import__('builtins'),'make_openai_responses_kwargs',(lambda m,t,e,p:{'model':m,'input':p,'max_output_tokens':t,'reasoning':{'effort':e}})) or 0")
!(py-eval "setattr(__import__('builtins'),'make_openai_embedding_kwargs',(lambda x:{'model':'text-embedding-3-large','input':x})) or 0")

!(change-state! &openai_client (py-call (openai.OpenAI)))

;Function to prompt GPT via OpenAI API:
(= (useGPT $prompt)
   (useGPT gpt-5.2 1000000 medium $prompt))
(= (useGPT $model $max_tokens $effort $prompt)
   (let* (($client    (get-state &openai_client))
          ($responses (py-call (getattr $client "responses")))
          ($create    (py-call (getattr $responses "create")))
          ($kwargs    (py-call (builtins.make_openai_responses_kwargs $model $max_tokens $effort $prompt)))
          ($res       (py-call (builtins.apply_kwargs $create $kwargs)))
          ($text      (py-call (getattr $res "output_text")))
          ($_ (cut)))
         $text))

;Function to prompt a model via OpenRouter API:
(= (useOpenRouter $model $prompt)
   (useOpenRouter $model 100000 medium $prompt))
(= (useOpenRouter $model $max_tokens $effort $prompt)
   (py-call (lib_llm.openrouter_chat $model $prompt $max_tokens $effort)))

(= (sread-safe $w)
   (case (catch (sread $w))
         (((Error $A $B) (empty))
          ($z $z))))

(= (useGPTEmbedding $text)
   (let* (($client     (get-state &openai_client))
          ($embeddings (py-call (getattr $client "embeddings")))
          ($create     (py-call (getattr $embeddings "create")))
          ($kwargs     (py-call (builtins.make_openai_embedding_kwargs $text)))
          ($res        (py-call (builtins.apply_kwargs $create $kwargs)))
          ($data       (py-call (getattr $res "data")))
          ($item       (py-call (builtins.index $data 0)))
          ($vector     (py-call (getattr $item "embedding")))
          ($_ (cut)))
         $vector))

(= (useOpenRouterEmbedding $model $text)
   (py-call (lib_llm.openrouter_embed $model $text)))
