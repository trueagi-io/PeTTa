; Regression for recompiling a clause whose nested match gets edge facts: all
; arms still construct PremiseBundle, so branch refinement must not erase the
; common result type or leave a runtime output guard.
(: Statement Type)
(: Goal Type)
(: TV Type)
(: Proof Type)
(: PremiseBundle Type)
(: CPU (-[det]-> Atom Atom Atom Goal))
(: premise-bundle (-[det]-> (List Goal) TV Proof PremiseBundle))
(: plain-tv TV)
(: plain-proof Proof)
(: plain-statement Statement)

(: bundle-after-match (-[det]-> Statement PremiseBundle))
(= (bundle-after-match $elem)
   (let $prem-kb plain-proof
      (if (is-var $elem)
         (premise-bundle () plain-tv $prem-kb)
         (if (= $elem (CPU $f $args $res))
            (premise-bundle ((CPU $f $args $res)) plain-tv $prem-kb)
            (premise-bundle () plain-tv $prem-kb)))))

!(test
   (bundle-after-match plain-statement)
   (premise-bundle () plain-tv plain-proof))
