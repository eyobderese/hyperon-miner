

! (register-module! ../../experiments)
! (register-module! ../../../hyperon-miner)
! (import! &database hyperon-miner:experiments:data:sample-data)
! (import! &self hyperon-miner:experiments:utils:common-utils)
 ! (import! &self experiments:rules:emp-tv-bs)


 ;; Rule to calculate the empirical truth value of a pattern.

 ;;;;;;;;;;
 ;; Type ;;
 ;;;;;;;;;;

(: TruthValue Type)

 ;;;;;;;;;;;;;;;;;;
 ;; Constructor ;;
 ;;;;;;;;;;;;;;;;;;

 ;; Takes pattern and db_ratio to form an
 ;; empirical truth value.
(: EMPTV (-> $a $b TruthValue))

(= (Default_k) 1)
(= (DEFAULT_K)  800.0)

 ;The count_to_confidence function calculates the confidence value based on a given count ($x).
 ;The confidence value is determined using a formula that incorporates a default constant (Default_k).
(= (count_to_confidence $x) (/ $x (+ $x (Default_k))))

 ;; calculate the empirical truth value of a pattern.
(= (emp-tv $pattern $db)
    (let*
        ( ( ($ucount ) ( (universe-count $pattern $db)))
            ( ($ms) ($ucount)) ;;  we didn't use ms for support calculation
            ( ($sup) ( (sup $pattern $db)))
            ( ($conf) ( (count_to_confidence $ucount)) )
            ($mean (/ $sup $ucount))
            ($confidence   (* 1e-1 $conf))
        )
    (EMPTV $mean  $confidence)))

 ; The prob_to_support function calculates the support of a given pattern in a database based on a provided probability estimate.
 ;The support is determined by multiplying the probability estimate by the universe count of the pattern in the database.
 ; Parameters
 ; $pattern: The pattern for which the support is to be calculated. This is typically a data structure or identifier representing the pattern.
 ; $db: The database in which the pattern's support is to be calculated. This is typically a collection or sequence of data(represented by space).
 ; $prob: The probability estimate of the pattern. This is a numeric value representing the likelihood of the pattern.
(= (prob_to_support $pattern $db $prob)
    (* $prob (universe-count $pattern $db))
)

 ; The support_estimate_calculator function calculates the support estimate for a given pattern in a database and then uses
 ; this estimate to compute the empirical truth value with bootstrapping if necessary.
 ;  It combines the probability estimate and database ratio to determine the effective database size and support estimate.
 ; Parameters
 ; $pattern: The pattern for which the support estimate and empirical truth value are to be calculated.
 ; $db: The database in which the pattern's support is to be calculated.
 ; $prob_estimate: The probability estimate of the pattern.
 ; $db_ratio: The ratio used to scale the database size.
(= (support_estimate_calculator $pattern $db $prob_estimate $db_ratio )

    (let* ( ($support_estimate (prob_to_support $pattern $db $prob_estimate ))
            ($database_size (* (db_size $db) $db_ratio)))
        (emp_tv_pbs $pattern $db $prob_estimate $db_ratio $database_size $support_estimate)))

 ;
 ;
 ; The categorize_subsize function categorizes the subsample size based on the comparison between the database size and the support estimate.
 ;  It returns a category identifier based on whether the support estimate is greater than or equal to the database size.
 ; Parameters
 ; $db_size: The size of the database.
 ; $support_estimate: The estimated support of a pattern in the database.
 ; Return Values
 ; 1: Returned if the support estimate is greater than or equal to the database size.
 ; 2: Returned if the support estimate is less than the database size.
(= (categorize_by_support_estimate $db_size $support_estimate)
    (if   (>= $support_estimate $db_size)
        1
        2)

)
 ;
 ;
 ;
 ;
 ; The emp_tv_pbs function calculates the empirical truth value of a pattern in a database, using bootstrapping if necessary.
 ; It categorizes the support estimate to decide the method for calculating the empirical truth value.

 ; Parameters
 ; $pattern: The pattern for which the empirical truth value is to be calculated.
 ; $db: The database in which the pattern's empirical truth value is to be calculated.
 ; $prob_estimate: The probability estimate of the pattern.
 ; $db_ratio: The ratio used to scale the database size.
 ; $db_size: The effective size of the database.
 ; $support_estimate: The estimated support of the pattern in the database.
 ; Return Values
 ; emp_tv_bs: Called if the support estimate is greater than or equal to the database size,
 ;  using bootstrapping to calculate the empirical truth value.
 ; emp_tv: Called if the support estimate is less than the database size, directly calculating the empirical truth value.
 ; emp_tv: Default case, directly calculating the empirical truth value.

(= (emp_tv_pbs $pattern $db $prob_estimate $db_ratio $db_size $support_estimate)
    (case (categorize_by_support_estimate  $db_size $support_estimate)
        (
            (1 (emp_tv_bs $pattern $db $db_size $db_ratio $support_estimate))
            (2 (emp-tv $pattern $db))
            ($_ (emp-tv $pattern $db))
        )))

 ;;Support: number of data trees matching a given pattern.
(= (sup $pattern $db)
    (tuple-count (collapse (match $db $pattern $pattern))))

 ;;ji_tv_est_mem this function is used to estimate the joint probability of a pattern.
 ;;ji_tv_est_mem is going to implemented in est-tv rule.

;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (=(do-ji-tv-est $db $pattern) (
;;    let $prtns (generet-partition-without-pattern $pattern)  
;;          ( let ($strength $confidence) (avrg_tv (ji-est-tv $db (db_size $db) $pattern $prtns) )
;;          ($pattern  (ETV $strength $confidence)))  
;;          ;;(ji-est-tv $db (db_size $db) $pattern $prtns)             
;; ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MOCKING ;;;;;;
;;;;;;;;;;;;;;;;;
( = (do-ji-tv-est $db $pattern) (                     
    $pattern  (ETV 0.6 0.9)                           
))                                                    
                                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (ji_tv_est $pattern $db) 
    (let* 
    (
        ($out_est (do-ji-tv-est $db $pattern)) 
        (($pat $tv) $out_est)
    )
    $tv
    )
)


 ;The categorize_subsize function categorizes a given subset size ($subsize) based on a threshold size ($ts).
 ;Parameters
 ;$subsize: The size of the subset to be categorized.
 ;$ts: The threshold size.
 ;Return Value
 ;Returns 2 if $subsize is greater than half of $ts.
 ;Returns 1 otherwise.
(= (categorize_subsize $subsize $ts)
    (if   (> $subsize (/ $ts 2))
        2
        1)

)

 ;; The copyDatabase function is designed to create a copy of the given database and return the new database.
 ;;This function ensures that all the contents of the original database are duplicated into a new database instance
 ;;copy-db function is designed to create a copy of the given database and return the new database.
; (= (copy-db $db $old-db-content)
;   (if (== $old-db-content ()) $db
;        (let* ( ($_ (add-atom $db (car-atom $old-db-content)))
;
;            )
;        (copy-db $db (cdr-atom $old-db-content))
;    )
;
;))

(= (copy-db $space $atoms) 
        (add-atom $space (superpose $atoms))
)

 ; The add_n_atoms_to_db function is designed to add a specified number of atoms to a given database. This function recursively adds atoms until the specified number is reached.
 ; Parameters
 ; $db: The database to which atoms will be added.
 ; $n: The number of atoms to add to the database.
(= (add_n_atoms_to_db $db $n)
    (if (<= $n 0) $db
        (let* ( ($_ (add-atom $db Pattern))

            )
        (add_n_atoms_to_db $db (- $n 1))
    )

)

)

 ; The copy-and-add-atom function is designed to create a copy of the given database and add a specified number of atoms to the new database. This function ensures that the contents of the original database are duplicated into a new database instance, and then a specified number of atoms are added to the new database.
 ; Parameters
 ; $db: The original database to be copied.
 ; $n: The number of atoms to add to the new database.
(= (copy-and-add-atom $db $n)

    (if (<= $n 0) $db
        (let* ( ($new-db (copy-db (new-space) ( (collapse (get-atoms $db)) ))))

        )
    (add_n_atoms_to_db $db $n)
)

)

 ; The duplicate-and-expand-space function is designed to create a copy of the given space and add a specified number of atoms to the new space. This function ensures that all the contents of the original space are duplicated into a new space instance, and then a specified number of atoms are added to the new space.
 ; Parameters
 ; $space: The original space to be copied.
 ; $n: The number of atoms to add to the new space.
(= (duplicate-and-expand-space $space $n)
    (let $new-db (copy-db  (new-space) (collapse (get-atoms $space) ) )
        (add_n_atoms_to_db $new-db $n)

    )
)
; ! (duplicate-and-expand-space (get-space))
 ;

 ; The copy-db-by-size function is designed to create a copy of the given database up to a specified size. This function ensures that the contents of the original database are duplicated into a new database instance until the specified size is reached.
 ; Parameters
 ; $db: The new database instance where the contents of the original database will be copied.
 ; $old-db-content: The contents of the original database that need to be copied.
 ; $n: The current count of atoms added to the new database.
 ; $size: The maximum number of atoms to be copied to the new database.
(= (copy-db-by-size $db $old-db-content $n $size)
    (if (or (== $old-db-content ()) (>= $n $size)) $db
        (let* ( ($_ (add-atom $db (car-atom $old-db-content))))
            (copy-db-by-size $db (cdr-atom $old-db-content) (+ $n 1) $size)
        )

)
)

 ; The subsample function adjusts the size of a given database to match a specified subset size ($subsize). If the current size of the database is greater than or equal to the subset size, it returns the original database. Otherwise, it categorizes the database and either adds or removes atoms to achieve the desired subset size.
 ; Parameters
 ; $db: The database to be subsampled.
 ; $subsize: The target subset size for the database.
(= (subsample $db $subsize)
    (if (>= (db_size $db) $subsize)
        $db
        (case (categorize_subsize  $subsize (db_size $db))
            (
                (1 (duplicate-and-expand-space $db (- (* $subsize 2) (db_size $db)))) ; if it is in group 1 add atoms to the database
                (2 (copy-db-by-size (new-space) (collapse (get-atoms $db ) ) 0 (- (* $subsize 2) (db_size $db))))
                ($_ (copy-db-by-size (new-space) (collapse (get-atoms $db ) ) 0 (- (* $subsize 2) (db_size $db))))
            )))) ; if it is in group 2 remove atoms from the database

 ; The do_emp_tv function calculates the empirical truth value for a given pattern in a database.
 ; It first processes the database to obtain a sequence of atoms, estimates the joint truth value, and then uses the support estimate calculator to compute the final empirical truth value.
 ; Parameters
 ; $pattern: The pattern for which the empirical truth value is to be calculated.
 ; $db: The database in which the pattern's empirical truth value is to be calculated.
 ; $db_ratio: The ratio used to scale the database size.
 
(= (do_emp_tv $pattern $db $db_ratio )
    (let* ( ($db_sequence (collapse (get-atoms $db) ))
            ($jte  (ji_tv_est $pattern $db))

             ;; after we get jte we must extract the mean
            ($mean (get-strength $jte)))

        (support_estimate_calculator  $pattern $db $mean $db_ratio))
)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (gen-random $max-value)  (
        (py-dot np-rnd randint) $max-value)
)

(= (get-element $index $list)
    (if (== $index 0)
        (car-atom $list) ; Return the first element if index is 0
        (get-element (- $index 1) (cdr-atom $list))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;; Implmented Before ;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (gen-random-subsample $db $subsize $space) ; must return a space which conteten the subsample only
    ( if (== (db_size $db) $subsize)
        $db
        (if (== $subsize 0)
            $space ; Return an empty list if the sample size is 0
            (let* (
                    ($db-element (collapse (get-atoms $db)))
                    ($random-index (gen-random $subsize)) ; Get a random index
                    ($element (get-element $random-index $db-element))
                    ($_ (add-atom $space $element)) ; Get the element from the list
                    ($tail (gen-random-subsample $db (- $subsize 1) $space) ) ; Recursively generate the rest of the s
                )
            $space))) ; Add the element to the subsample
)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;; subsample-py : subsample with python binding ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (subsample-py $db $subsize )
    (let* (
            ($db-content (collapse (get-atoms $db)))
            ($random-selected (generet_random_subsample $db-content $subsize))
            ($space (copy-db  (new-space) $random-selected))
        )
    $space
)
)

(= (emp-tv-subsmp $pattern $db $subsize) (
        let* ( ($space (subsample-py $db $subsize (new-space))))
        ( emp-tv $pattern $space)
    ))

 ; Helper function to repeatedly resample and collect empirical truth values
(= (emp-tv-bs-helper $pattern $db $n-resample $subsize)
    (if (== $n-resample 0)
        () ; Base case: no more resampling, return an empty list
        (let* (
                ($subsample-tv (emp-tv-subsmp $pattern $db $subsize)) ; Compute empirical truth value for subsample
                ($remaining-esstvs (emp-tv-bs-helper $pattern $db (- $n-resample 1) $subsize)) ; Recursive call
            )
        (cons-atom $subsample-tv $remaining-esstvs)))) ; Add the result to the list and continue

 ; Function: subsmp-size
 ; Arguments:
 ;   $pattern - The pattern to be matched.
 ;   $db-size - The size of the database.
 ;   $support-estimate - The estimated support value.
 ;   $min-subsize - The minimum sub-sample size.
 ; Description:
 ;   This function calculates the sub-sample size based on the given pattern, database size,
 ;   support estimate, and minimum sub-sample size. It first computes the number of conjuncts
 ;   in the pattern ($nc) and the alpha value ($alpha) which is the ratio of support estimate
 ;   to the database size raised to the power of $nc. Then, it calculates the result ($res)
 ;   by raising the ratio of database size to ten times alpha to the power of 1/$nc. Finally,
 ;   it returns the maximum value between $res and the minimum of $min-subsize and $db-size.

(= (subsmp-size $pattern $db-size $support-estimate)
    (let* ( ($nc (n_conjuncts $pattern))    
            (($min-size) (10))   
            ($alpha (/ $support-estimate (pow-py $db-size $nc)))
            ($base (/ $db-size (* 10 $alpha)))
            ($power (/ 1 $nc))
            ($res (pow-py $base $power))
    )
        (max $res (min $min-size $db-size))
    )
)

 ; Function to calculate empirical truth value with bootstrapping (resampling)

 ;;TODO: it should be updated.
(= (emp-tv-bs $pattern $db $db_size $db_ratio $support_estimate) (
        let* (
            ($_ (println BSStarted))
            ($db-size-ratio (* $db_size $db_ratio))
            ($subsize (subsmp-size $pattern $db-size-ratio $support_estimate))
            ($n-resample 10)
        )
    (emp-tv-bs $pattern $db $n-resample $subsize)

) ) ; If subsize >= db size, directly return empirical truth value

(= (emp-tv-bs $pattern $db $n-resample $subsize)

    (if (< $subsize (db_size $db))
        (let* (
                ($esstvs (emp-tv-bs-helper $pattern $db $n-resample $subsize))
            )
        (mean-tv $esstvs)) ; Return average of the empirical truth values
    (emp-tv $pattern $db))) ; If subsize >= db size, directly return empirical truth value

 ;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;  avrg-tv     ;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; Calculate the average of EMTV:
 ; 1. Convert each TV into a β distribution.
 ; 2. Calculate the mean of each β distribution.
 ; 3. Calculate the variance of each β distribution.
 ; 4. Calculate the average of the means of the β distributions.
 ; 5. Calculate the relative variance of each variance using the Mixture distribution.
 ; 6. Calculate the average of the relative variances.

 ; A function to get the beta distribution (α and β) from a truth value
 ;  // Formul from PLN book.
 ; confidence_t cf = std::min(_value[CONFIDENCE], 0.9999998);
 ; return static_cast<count_t>(DEFAULT_K * cf / (1.0 - cf));

(= (max $a $b) (if (> $a $b) $a $b))
(= (min $a $b) (if (< $a $b) $a $b))

(= (get-strength ($type $strength $confidence)) $strength)
(= (get-confidence ($type $strength $confidence)) $confidence)

(= (get-count $confidence) (
        let $cf (min 0.9999998 $confidence)
        (/ (* (DEFAULT_K) $cf)  (- 1.0 $cf))
    ))

 ; This function calculates the parameters of a Beta distribution based on the given empirical TV (emptv) data.
 ; It extracts the strength, confidence, and count from the empirical data, then computes the positive count
 ; and uses prior alpha and beta values (both set to 1.0) to calculate the final alpha and beta parameters
 ; for the Beta distribution. The final alpha is the sum of the prior alpha and the positive count, while the
 ; final beta is the sum of the prior beta and the difference between the count and the positive count.

(= (BetaDistribution $emptv)
    (let* (

            ($strength (get-strength $emptv))
            ($confidence (get-confidence $emptv))
            ($count (get-count $confidence))
            ($pos_count (* $strength $count))
            ($p_alpha 1.0)
            ($p_beta 1.0)
            ($alpha (+ $p_alpha $pos_count))
            ($beta (+ $p_beta (- $count $pos_count)))
             ; ($α (* $strength $confidence))
             ; ($β (* (- 1 $strength) $confidence))
        )
    ($alpha $beta)
))

 ; Function to compute the mean of the Beta distribution
(= (mean-β-dis ($α $β))

    (/ $α (+ $α $β))
)

 ; Function to compute the variance of the Beta distribution
(= (variance-β-dis ($α $β))

    (/ (* $α $β)
        (*
            (* (+ $α $β) (+ $α $β))
            (+ (+ $α $β) 1)
        )
)
)

 ; Function to calculate the average mean
(= (avrg-means $means)
    (/ (sum $means) (tuple-count $means))
)

 ; Function to calculate the average variance
(= (avrg-variances $variances)
    (/ (sum $variances) (tuple-count $variances))
)

 ; Function to compute the relative variances

(= (relative-variance $means $variances $mean)
    (let
         ; Use map to iterate over means and variances simultaneously
        $adjusted-variances
        (relateive-varaince-helper
            $means
            $variances
            $mean
        )
     ; Return the adjusted variances
    $adjusted-variances
)
)

(= (relateive-varaince-helper $means $variances $mean)
    (if (or (== $means ()) (== $variances ())) ()
        (let* ( ($meanHead (car-atom $means))
                ($meanTail (cdr-atom $means))
                ($varianceHead (car-atom $variances))
                ($varianceTail (cdr-atom $variances))
                ($head-new  (+ $varianceHead (* (- $meanHead $mean) (- $meanHead $mean))))
                ($tail-new (relateive-varaince-helper $meanTail $varianceTail $mean))
            )
        (cons-atom $head-new $tail-new)
    )
)
)

(= (combine-varaince $variances $relateive-varaince)
    (if (or (== $relateive-varaince ()) (== $variances ())) ()
        (let* ( ($varianceHead (car-atom $variances))
                ($varianceTail (cdr-atom $variances))
                ($reVarHead (car-atom $relateive-varaince))
                ($reVarTail (cdr-atom $relateive-varaince))

                ($head-new  (+ $varianceHead $reVarHead))
                ($tail-new (combine-varaince $varianceTail $reVarTail))
            )
        (cons-atom $head-new $tail-new)
    )
)
)

 ; This function `mk-stv` creates a standard truth value (STV) using the provided mean and variance.
 ; It calculates the alpha and beta parameters based on the mean and variance.
 ; It then computes the count and confidence values using these parameters.
 ; The mode is determined based on the values of alpha and beta, following specific conditions.
 ; Finally, it returns an EMPTV (mode, confidence) pair.
(= (mk-stv $mean $variance)
    (let* (
            ($alpha (* $mean (/ (* $mean (- 1 $mean)) $variance)))
            ($beta (* (- 1 $mean) (/ (* $mean (- 1 $mean)) $variance)))
            ($p_alpha 1.0)
            ($p_beta 1.0)
            ($count (max 0.1 (- (- (+ $alpha $beta ) $p_alpha) $p_beta)))
            ($confidence (/ $count (+ $count (DEFAULT_K))))
            ($mode  (
                    if (and (> $alpha 1) (> $beta 1))
                    (/ (- $alpha 1) (- (+ $alpha $beta) 2))
                    (if (and (<= 1 $alpha) (<= 1 $beta))
                        1
                        (if (and (< $alpha 1) (<= 1 $beta))
                            0
                            (if (and (<= 1 $alpha) (< $beta 1))
                                1
                                $mean
                            )
                    )
            )
    ))

)
(EMPTV $mode $confidence))

)

 ; Orchestrating the overall process to calculate the average truth value

 ;  * This function calculates the average truth value from a list of truth values.
 ;  *
 ;  * Steps followed in the calculation:
 ;  *
 ;  * 1. Extract Beta distributions from the given truth values.
 ;  * 2. Calculate the mean for each Beta distribution.
 ;  * 3. Calculate the variances for each Beta distribution.
 ;  * 4. Calculate the overall average mean from the individual means.
 ;  * 5. Calculate the relative variances using the individual means, variances, and the overall average mean.
 ;  * 6. Combine the original variances with the relative variances.
 ;  * 7. Calculate the average variance from the combined variances.
 ;  * 8. Create the final truth value using the average mean and variance.
 ;  * 9. Return the final truth value.

(= (avrg-tv $tvs)
    (let* (
             ; Extract Beta distributions from truth values
            ($beta-dists (mapper BetaDistribution $tvs))

            ($_ (println! (Beta-distribution-is $beta-dists)))

             ; Calculate the mean for each Beta distribution
            ($means (mapper mean-β-dis $beta-dists))
            ($_ (println! (means-is $means)))

             ; Calculate the variances for each Beta distribution
            ($variances (mapper variance-β-dis $beta-dists))
            ($_ (println! (variance-is $variances)))

             ; Calculate the overall average mean
            ($mean (avrg-means $means))
            ($_ (println! (mean-is $mean)))

             ; Calculate relative variances
            ($relative-variances (relative-variance $means $variances $mean))

             ; Add relative variances to the variances
            ($combined-variances (combine-varaince $variances $relative-variances))
             ; Calculate the average variance
            ($variance (avrg-variances $combined-variances))

             ; Create the final truth value with the average mean and variance
            ($_ (println! ($mean $variance)))
            ($final-tv (mk-stv $mean $variance))
             ; ($final-tv (mk-stv $mean $variance))
        )
    $final-tv
))


;; ;===================Experiment=================================

! (add-reduct &self (= (get-space) (new-space)))
! (add-atom (get-space) (Inheritance Abe human))
! (add-atom (get-space) (Inheritance Rio human))
! (add-atom (get-space) (Inheritance Bob human))
! (add-atom (get-space) (Inheritance Mike human))
! (add-atom (get-space) (Inheritance Mike Nil))
! (add-atom (get-space) (Inheritance Zac human))
! (add-atom (get-space) (Inheritance Zac human))
! (add-atom (get-space) (Parent Abe Nil ))

; !(get-atoms (subsample-py (get-space) 3))

; !(emp-tv-subsmp (Inheritance Zac $x) (get-space) 4)

; !(subsmp-size (Inheritance Zac $x) 8 0.5)

; !(emp-tv-bs (Inheritance Zac $x) (get-space) 8 0.6 0.9) 

; !(BetaDistribution (EMPTV 0.9773269689737469 0.0009825445253591023) )

; !(mk-stv 1.7689693569125715 1.0178393379503528 )
; !(ji_tv_est (Patern Ryo Ri) &self)

; !(do_emp_tv (Inheritance Zac Eyob) (get-space) 0.8)