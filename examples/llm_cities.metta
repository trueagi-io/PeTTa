!(import! &self lib/lib_llm)

;GPT LLM use case, extract country cities are located in:
!(let* (($city (superpose (stockholm vienna)))
        ($sexpr (sread-safe (useGPT (py-str ("in which country is " $city "? Answer via S-Expression (in CITY COUNTRY), output in lowercase and do not output anything else"))))))
       (add-atom &self $sexpr))

;Test for inserted knowledge:
!(test (collapse (match &self (in $1 $2) (in $1 $2)))
       ((in stockholm sweden) (in vienna austria)))
