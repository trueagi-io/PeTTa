(= (useGPT $model $max_tokens $reasoning $prompt)
   (call-llm "OpenAI" $prompt $max_tokens))

(= (useGPTEmbedding $text)
   (call-embedding $text "OpenAI"))

(= (str $a $b)
   (atomic_list_concat ($a $b)))
