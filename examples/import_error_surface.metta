!(import! &self ../lib/lib_he)

!(test (if-error (catch (import! &self imports/import_error_broken))
                 Error
                 NoError)
       Error)

!(test (if-error (catch (import! &self imports/definitely_missing_import))
                 Error
                 NoError)
       Error)
