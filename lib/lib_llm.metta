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

;Add to builtins a function that POST JSON to OpenRouter and return the assistant reply text
!(py-eval "setattr(__import__('builtins'),'or_post',(lambda model,prompt,key: __import__('json').loads(__import__('urllib.request',fromlist=['urlopen','Request']).urlopen(__import__('urllib.request',fromlist=['Request']).Request('https://openrouter.ai/api/v1/chat/completions',__import__('json').dumps({'model':model,'messages':[{'role':'user','content':prompt}]}).encode('utf-8'),{'Authorization':'Bearer '+key,'Content-Type':'application/json'})).read())['choices'][0]['message']['content'])) or 0")
;Add to builtins a function that POST to OpenRouter embeddings endpoint and return the embedding vector
!(py-eval "setattr(__import__('builtins'),'or_embed',(lambda model,text,key: __import__('json').loads(__import__('urllib.request',fromlist=['urlopen','Request']).urlopen(__import__('urllib.request',fromlist=['Request']).Request('https://openrouter.ai/api/v1/embeddings',__import__('json').dumps({'model':model,'input':text}).encode('utf-8'),{'Authorization':'Bearer '+key,'Content-Type':'application/json'})).read())['data'][0]['embedding'])) or 0")

;Function to prompt GPT via OpenAI API:
(= (useGPT $prompt)
   (useGPT gpt-5.2 1000000 medium $prompt))
(= (useGPT $model $max_tokens $effort $prompt)
   (let* (($client    (py-call (openai.OpenAI)))
          ($responses (py-call (getattr $client "responses")))
          ($create    (py-call (getattr $responses "create")))
          ($mydict (py-str ("{" "'model':'" $model "'," "'input':\"" $prompt "\","
                            "'max_output_tokens':" $max_tokens "," "'reasoning':{'effort':'" $effort "'}" "}")))
          ($kwargs (once (py-eval $mydict)))
          ($res    (py-call (builtins.apply_kwargs $create $kwargs)))
          ($text   (py-call (getattr $res "output_text")))
          ($_ (cut)))
     $text))

;Function to prompt a model via OpenRouter API:
(= (useOpenRouter $model $prompt)
   (let* (($apikey (once (py-eval "__import__('os').environ.get('OPENROUTER_API_KEY','')")))
          ($text (once (py-call (builtins.or_post $model $prompt $apikey)))))
         $text))

(= (sread-safe $w)
   (case (catch (sread $w))
         (((Error $A $B) (empty))
          ($z $z))))

(= (useGPTEmbedding $text)
   (let* (($client     (py-call (openai.OpenAI)))
          ($embeddings (py-call (getattr $client "embeddings")))
          ($create     (py-call (getattr $embeddings "create")))
          ($mydict     (py-str ("{'model':'text-embedding-3-large','input':\"" $text "\"}")))
          ($kwargs     (once (py-eval $mydict)))
          ($res        (py-call (builtins.apply_kwargs $create $kwargs)))
          ($data       (py-call (getattr $res "data")))
          ($item       (py-call (builtins.index $data 0)))
          ($vector     (py-call (getattr $item "embedding")))
          ($_ (cut)))
         $vector))

(= (useOpenRouterEmbedding $model $text)
   (let* (($apikey (once (py-eval "__import__('os').environ.get('OPENROUTER_API_KEY','')")))
          ($vector (once (py-call (builtins.or_embed $model $text $apikey)))))
         $vector))
