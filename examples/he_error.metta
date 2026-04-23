!(import! &self lib/lib_he)

!(test (let $result (catch (+ 40 2))
       (if-error $result
           Error
           $result))
       42)

!(test (let $result (catch (+ 40 a))
            (if-error $result
                Error 
                $result))
       Error)

!(test (if-error (Error 5 BadType) "Error!" "No error")
       "Error!")

!(test (return-on-error (Error 5 BadType) 6)
       (Error 5 BadType))

!(test (return-on-error 5 6) 6)
