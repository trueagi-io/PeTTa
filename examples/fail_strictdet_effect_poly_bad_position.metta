; v1 effect variables may decorate only the top arrow and direct closure
; parameter arrows. This nested result arrow is deliberately illegal.
(: nested-effect-position
   (-[$e]-> (-[$e]-> Number (-[$e]-> Number Number)) Number))
