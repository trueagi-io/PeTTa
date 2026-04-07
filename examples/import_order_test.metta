;Test that import order does not matter:
;import_order.metta uses is-even from import_order_helper.metta,
;but is imported first (before its dependency).
!(import! &self import_order)
!(import! &self import_order_helper)

; Test process-number 
!(test (process-number 10) 25)  
!(test (process-number 15) 30)  


