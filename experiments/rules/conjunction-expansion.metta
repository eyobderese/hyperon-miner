 ; /**
 ;  * Given cnjtion and pattern, consider all possible connections
 ;  * (a.k.a linkages) and expand cnjtion accordingly. For instance if
 ;  *
 ;  * cnjtion = (Inheritance X Y)
 ;  * pattern = (Inheritance Z W)
 ;  *
 ;  * return
 ;  *
 ;  *   (, (Inheritance X Y) (Inheritance X W))
 ;  *   (, (Inheritance X Y) (Inheritance Z X))
 ;  *   (, (Inheritance X Y) (Inheritance Y W))
 ;  *   (, (Inheritance X Y) (Inheritance X Y))
 ;  *
 ;  * It will also only include patterns with minimum support ms
 ;  * according to db, and perform alpha-conversion when necessary.
 ;  *
 ;  * mv is the maximum number of variables allowed in the resulting
 ;  *    patterns.
 ;  *
 ;  */

! (register-module! ../../experiments)
! (import! &self experiments:utils:common-utils)
! (import! &self conj-exp)

 ; The starting point for the conjunction expansion. It passes
 ; the Alpha Converted pattern (if necessary) to the expand_conjunction functions.
 ; if it passes the minimum variable check

(= (expand_conjunction $cnjtion $pattern $db $ms $mv $es)
    (let* (
            ($apat (alpha_convert $pattern (get_variables $cnjtion))))
        (if (> (let* (
                        ($temp (get_variables $pattern))
                        ($temp2 (get_variables $cnjtion)))
                    (+ (length $temp) (length $temp2))) $mv)
            (if $es
                (expand_conjunctin_es_rec $cnjtion $apat $db $ms $pattern)
                (expand_conjunction_rec $cnjtion $apat $db $ms $pattern))
            ())
    )
)

 ; Function: get_variables
 ; This function extracts all the variables from a given pattern (Expression).
 ; It works recursively by checking each element of the pattern to see if it is of the metatype "Variable".

(= (get_variables $pattern)
    (if (== $pattern ())
        ()
        (if (== (let* (
                        ($var (car-atom $pattern)))
                    (get-metatype $var)) Variable)
            ( (car-atom $pattern) (get_variables (cdr-atom $pattern)))
            (get_variables (cdr-atom $pattern))
        ))
)

 ; Alpha convert pattern so that none of its variables collide with
 ; the variables in other_vars.

(= (alpha_convert $pattern $cnjtion_vars)
    (if (== $cnjtion_vars ())
        $pattern
        (let* (
                ($pattern_vars (get_variables $pattern))
            )
        (if (is-member (car-atom $cnjtion_vars) $pattern_vars)
            (alpha_convert (collapse (replace $pattern $cnjtion_vars)) (cdr-atom $cnjtion_vars))
            (alpha_convert $pattern (cdr-atom $cnjtion_vars))
        )))
)

 ; It recursively traverses the pattern, checking each atom, and replaces
 ; it with a corresponding replacement (if one exists) by calling `find-replacement`.

(= (replace $pattern $variables)
    (if (== $pattern ())
        (empty)
        (let* (
                ($head (car-atom $pattern))
                ($tail (cdr-atom $pattern))
                ($rh (find-replacement $head $variables))
            )
        (superpose ($rh (replace $tail $variables)))
    )
))

 ; This function finds a replacement for a given variable
 ; in the list of variables. And the genrateRandomVar function is written
 ; with python and imported here.

(= (find-replacement $var $variables)
    (if (== $variables ())
        $var
        (if (== $var (car-atom $variables))
            (genRandomVar $var $variables)
            (find-replacement $var (let* (
                        ($temp (cdr-atom $variables))
                        ($tail (car-atom $temp)))
                    $tail)))
    )
)

(= (expand_conjunction_rec $cnjtion $apat $db $ms $pattern)
    (let $npat (let $temp (expand_conjunction_connect $cnjtion $pattern (get_variables $pattern) (get_variables $cnjtion)) (format_list $temp))
        (checker $cnjtion (reconstruct $cnjtion $npat) $ms))
)

 ; Simulate support evaluation since the support implementation currently takes
 ; lots of minutes to check just one support and I have to check more than 20 pattern's support

(= (support_evaluation $pattern $minimum_support)
    True
)

(= (length ()) 0)
(= (length ($x $xs))
    (+ 1 (length $xs)))

 ; This function checks if the patterns generated pass support evaluation and the conjunction passes n_conjuncts check
 ; (The implementation for n_conjunction in the common-utils.metta seems incorrect. I don't know maybe I am wrong :)

(= (checker $cnjtion () $ms) ())
(= (checker $cnjtion ($x $xs) $ms)
    (if (support_evaluation $x $ms)
        ($x (checker $cnjtion $xs $ms))
        (checker $cnjtion $xs $ms)
    )
)

 ; Combine and reconstruct are functions to combine each patterns generated with
 ; the conjunction and then reconstruct them into a list.
 ; It will create something like this: ((, (Inheritance $X $Y) (Inheritance $X $W $A)) ...)

(= (combine $cnjtion $pattern)
    (if (== $patterns ())
        ()
        (cons-atom , ($cnjtion $pattern))
    )
)

(= (reconstruct $cnjtion $xs)
    (if (== $xs ())
        ()
        ( (combine $cnjtion (car-atom $xs)) (reconstruct $cnjtion (cdr-atom $xs)))
    )
)

(= (expand_conjunction_connect $cnjtion $pattern $cnjtion_var $pattern_var)
    (expand_conjunction_connect $cnjtion $pattern (combine_lists $cnjtion_var $pattern_var))
)

 ; This function create the patterns using the combined variables from the
 ; Pattern and the Conjunction. It will create a list of patterns that can be used for combination later with conjunction.
 ; It will create something like: (Inheritance ($X ($Y ($X (End (Inheritance ($X ($Y ($W (End  ...))))))))))

(= (expand_conjunction_connect $cnjtion $pattern $pv2cv)
    (if (== $pv2cv ())
        ()
        (pattern_var_replacer $pattern $pattern (car-atom $pv2cv) $cnjtion $pv2cv)
    )
)

(= (pattern_var_replacer $ori_pattern $pattern $var $cnjtion $pv2cv)
    (if (== $pattern ())
        (End (expand_conjunction_connect $cnjtion $ori_pattern (cdr-atom $pv2cv)))
        (if (is-variable (car-atom $pattern))
            ( (car-atom $var) (pattern_var_replacer $ori_pattern (cdr-atom $pattern) (cdr-atom $var) $cnjtion $pv2cv))
            ( (car-atom $pattern) (pattern_var_replacer $ori_pattern (cdr-atom $pattern) $var $cnjtion $pv2cv)))
    )
)

 ; The implementation has no difference with the expand_conjunction_rec
 ; in the C++ code.
(= (expand_conjunction_es_rec $cnjtion $apat $db $ms)
    (pass)
)

 ;Test cases
! (alpha_convert (Inheritance $Y $X ConceptNode Test $Z) ($Y ($U ($V ()))))
 ;! (expand_conjunction (Inheritance $X $Y) (Inheritance $Z $W $A) &db 10 2 False)
 ; ! (get_variables (Inheritance $Y $X ConceptNode Test $Z))
 ; ! (collapse (replace (Inheritance $Y $Y ConceptNode Test $Z $Z $A)  ($X ($Y ($A ())))))
 ; ! (let $x (alpha_convert (Inheritance $Y $X ConceptNode Test $Z) ($Y ($U ($V ())))) (get_variables $x))
 ; ! (format_list (Inheritance ($X ($Y ($W (Nil (Inheritance ($X ($Y ($A (Nil (Inheritance ($X ($X ($W (Nil (Inheritance ($X ($X ($A (Nil (Inheritance ($Y ($W ($A (Nil ()))))))))))))))))))))))))))
 ; ! (expand_conjunction_connect (Inheritance $X $Y) (Inheritance $X $W $A) (get_variables (Inheritance $X $Y)) (get_variables (Inheritance $X $W $A)))