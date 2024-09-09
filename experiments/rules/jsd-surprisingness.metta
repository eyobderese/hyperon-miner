 ;  ;; Calculate the surprisingness of a pattern as the Jensen-Shannon
 ;  ;; Distance between its empirical truth value and its truth value
 ;  ;; estimate, based a reasoning (typically excluding the emp rule).

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; We can use Nodes and Links in the OpenCog Classic style,
 ; because these are also symbolic expressions,
 ; although Concept and List will not be turned into types
 ; and will remain just symbols in expressions
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; (EvaluationLink
 ;     (PredicateNode "eats")
 ;     (ListLink
 ;         (ConceptNode "Sam")
 ;         (ConceptNode "flies")))

! (register-module! ../../experiments)
! (import! &self  jsd-surprisingness)

(: mapper (-> (-> $t $t) Expression Expression))
(= (mapper $f $expr)
    (if (== $expr ()) ()
        (let* ( ($head (car-atom $expr))
                ($tail (cdr-atom $expr))
                ($head-new ($f $head))
                ($tail-new (mapper $f $tail))
            )
        (cons-atom $head-new $tail-new)
    )
)
)

 ;  ;; Generate a rule to calculate the JSD-Surprisingness of a pattern
 ;  ;; that is the conjunction of nary components.
(= (gen-jsd-surprisingness-rule $nary)
    (
        (if (< 1 $nary)
            (let* ( ($cnj_bodies (gen-variables "$cnj-bodies" $nary))
                    ($f (quote
                            (Lambda
                                ($f_vardecl)
                                (mapper Present ($cnj_bodies)))))
                    ($minsup_e (minsup-eval $f db ms))
                    ($jsd_e (jsd-eval $f db))
                    ($surp_e (surp-eval 'jsdsurp $f $db))

                )
            (if (and ( and (absolutely_true_eval $minsup_e) (gt_zero_confidence_eval $jsd_e)) (and (and (Present $minsup_e)  (Present $jsd_e)) (Absent $surp_e)) )
                (jsd-surprisingness-formula $surp_e $minsup_e $jsd_e ) (empty))

        )

    ())))
(= (jsd-surprisingness-formula $surp_e $minsup_e $jsd_e  )

    (let* ( ($jsdsurp-e $surp_e)
            ($surp ($cog-mean $jsd_e))
        )
    (cog-set-tv! $jsdsurp-e (stv $surp 1))
))

! (gen-variables "$cnj-bodies" 4)
!  (mapper Present ($cnj-bodies-0 $cnj-bodies-1))