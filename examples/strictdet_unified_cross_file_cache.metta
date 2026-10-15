!(import! &self (library lib_unified_cache_fixture))

(: unified-cache-bool-consumer (-[det]-> Bool Bool))
(= (unified-cache-bool-consumer $value)
   (and (unified-cache-bool-case $value) true))

!(test (unified-cache-bool-consumer true) true)
