!(import! &self (library lib_builtin_types))

(: TV Type)
(: exec-tv (-> (| (CPU %Undefined% %Undefined% %Undefined%)
                  (: %Undefined% %Undefined% %Undefined% TV))
               TV))
(= (exec-tv $e) (mkTV 0))

; 42 fits no union member: rejected at compile time.
!(exec-tv 42)
