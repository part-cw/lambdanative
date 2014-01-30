;; The line patterns for each possible character

(define pattern_a (list (list (list #t #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                        (list (list #t #t #t #f) (list #f #t #f #t) (list #t #f #t #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_b8 (list (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                         (list (list #t #t #t #f) (list #f #t #f #t) (list #t #f #t #t))
                         (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_c (list (list (list #t #t #f #f) (list #f #t #f #t) (list #f #f #f #t))
                        (list (list #t #f #t #f) (list #f #f #f #f) (list #f #f #f #f))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #f #t))))

(define pattern_do0 (list (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                          (list (list #t #f #t #f) (list #f #f #f #f) (list #t #f #t #f))
                          (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_e (list (list (list #t #t #f #f) (list #f #t #f #t) (list #f #f #f #t))
                        (list (list #t #t #t #f) (list #f #t #f #t) (list #f #f #f #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #f #t))))

(define pattern_f (list (list (list #t #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                        (list (list #t #t #t #f) (list #f #t #f #t) (list #f #f #f #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #f #t))))

(define pattern_g (list (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                        (list (list #t #f #t #f) (list #f #t #f #f) (list #f #f #t #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #f #t))))

(define pattern_h (list (list (list #t #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                         (list (list #t #t #t #f) (list #f #t #f #t) (list #t #f #t #t))
                         (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_i (list (list (list #f #t #f #f) (list #t #t #f #t) (list #f #f #f #t))
                        (list (list #f #f #f #f) (list #t #f #t #f) (list #f #f #f #f))
                        (list (list #f #t #f #f) (list #f #t #t #t) (list #f #f #f #t))))

(define pattern_j (list (list (list #t #t #f #f) (list #t #f #f #t) (list #f #f #f #f))
                        (list (list #f #f #t #f) (list #t #f #t #f) (list #f #f #f #f))
                        (list (list #f #t #f #f) (list #f #t #t #t) (list #f #f #f #t))))

(define pattern_k (list (list (list #t #f #f #f) (list #t #t #f #f) (list #f #f #f #f))
                        (list (list #t #t #t #f) (list #t #f #t #t) (list #f #f #f #f))
                        (list (list #f #f #t #f) (list #f #t #t #f) (list #f #f #f #f))))

(define pattern_l (list (list (list #t #t #f #f) (list #f #t #f #t) (list #f #f #f #t))
                        (list (list #t #f #t #f) (list #f #f #f #f) (list #f #f #f #f))
                        (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_m (list (list (list #t #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                        (list (list #t #f #t #f) (list #t #f #f #f) (list #t #f #t #f))
                        (list (list #f #t #t #f) (list #f #t #t #t) (list #f #f #t #t))))

(define pattern_n (list (list (list #t #f #f #f) (list #t #t #f #f) (list #t #f #f #t))
                        (list (list #t #f #t #f) (list #t #f #t #f) (list #t #f #t #f))
                        (list (list #f #t #t #f) (list #f #f #t #t) (list #f #f #t #f))))

(define pattern_p (list (list (list #t #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                        (list (list #t #t #t #f) (list #f #t #f #t) (list #t #f #f #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_q (list (list (list #f #f #f #f) (list #t #t #f #f) (list #f #f #f #t))
                        (list (list #t #t #f #f) (list #f #t #t #t) (list #t #f #f #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_r (list (list (list #t #f #f #f) (list #t #t #f #f) (list #f #f #f #t))
                        (list (list #t #t #t #f) (list #f #t #t #t) (list #t #f #f #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_s5 (list (list (list #f #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                         (list (list #t #t #f #f) (list #f #t #f #t) (list #f #f #t #t))
                         (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #f #t))))

(define pattern_t (list (list (list #f #f #f #f) (list #t #f #f #f) (list #f #f #f #f))
                        (list (list #f #f #f #f) (list #t #f #t #f) (list #f #f #f #f))
                        (list (list #f #t #f #f) (list #f #t #t #t) (list #f #f #f #t))))

(define pattern_uv (list (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                         (list (list #t #f #t #f) (list #f #f #f #f) (list #t #f #t #f))
                         (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_w (list (list (list #t #t #f #f) (list #t #t #f #t) (list #t #f #f #t))
                        (list (list #t #f #t #f) (list #f #f #f #f) (list #t #f #t #f))
                        (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_x (list (list (list #f #t #t #f) (list #t #t #f #t) (list #f #f #t #t))
                        (list (list #t #t #f #f) (list #f #t #t #t) (list #t #f #f #t))
                        (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_y (list (list (list #f #f #f #f) (list #t #f #f #f) (list #f #f #f #f))
                        (list (list #t #t #f #f) (list #f #t #t #t) (list #t #f #f #t))
                        (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_z (list (list (list #f #f #f #f) (list #t #t #f #f) (list #f #f #f #t))
                        (list (list #f #f #f #f) (list #t #f #t #f) (list #f #f #f #f))
                        (list (list #f #t #f #f) (list #f #f #t #t) (list #f #f #f #f))))

(define pattern_1 (list (list (list #f #f #f #f) (list #t #t #f #t) (list #f #f #f #f))
                        (list (list #f #f #f #f) (list #t #f #t #f) (list #f #f #f #f))
                        (list (list #f #f #f #f) (list #f #f #t #t) (list #f #f #f #f))))

(define pattern_2 (list (list (list #t #t #f #f) (list #f #t #f #t) (list #f #f #f #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #t #f #f #t))
                        (list (list #f #t #f #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_3 (list (list (list #f #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                        (list (list #f #t #f #f) (list #f #t #f #t) (list #t #f #t #t))
                        (list (list #f #t #f #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_4 (list (list (list #f #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                        (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #t #t))
                        (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_6 (list (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #f #t))
                        (list (list #t #t #t #f) (list #f #t #f #t) (list #f #f #t #t))
                        (list (list #f #t #t #f) (list #f #f #f #t) (list #f #f #f #f))))

(define pattern_7 (list (list (list #f #f #f #f) (list #t #f #f #f) (list #f #f #f #f))
                        (list (list #f #f #f #f) (list #f #t #t #f) (list #t #f #f #t))
                        (list (list #f #t #f #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_9 (list (list (list #f #f #f #f) (list #f #t #f #f) (list #t #f #f #t))
                        (list (list #t #t #f #f) (list #f #t #f #t) (list #t #f #t #t))
                        (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_exc (list (list (list #f #f #f #f) (list #f #t #f #t) (list #f #f #f #f))
                          (list (list #f #f #f #f) (list #t #f #t #f) (list #f #f #f #f))
                          (list (list #f #f #f #f) (list #f #f #t #f) (list #f #f #f #f))))

(define pattern_at (list (list (list #t #t #f #f) (list #f #t #f #t) (list #f #f #f #t))
                         (list (list #t #f #t #f) (list #f #t #f #t) (list #t #f #f #t))
                         (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_number (list (list (list #t #t #t #t) (list #f #t #f #t) (list #t #t #t #t))
                             (list (list #t #f #t #f) (list #f #f #f #f) (list #t #f #t #f))
                             (list (list #t #t #t #t) (list #f #t #f #t) (list #t #t #t #t))))

(define pattern_dollar (list (list (list #f #t #f #f) (list #t #t #t #t) (list #t #f #f #t))
                             (list (list #t #t #f #f) (list #t #t #t #t) (list #f #f #t #t))
                             (list (list #f #t #t #f) (list #t #t #t #t) (list #f #f #f #t))))

(define pattern_perc (list (list (list #t #f #f #f) (list #f #t #f #f) (list #f #f #f #t))
                           (list (list #f #t #t #f) (list #f #t #f #t) (list #t #f #f #t))
                           (list (list #f #t #f #f) (list #f #f #f #t) (list #f #f #t #f))))

(define pattern_hat (list (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                          (list (list #t #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                          (list (list #f #t #t #f) (list #f #t #f #t) (list #f #f #t #t))))

(define pattern_amp (list (list (list #t #t #f #f) (list #t #t #f #t) (list #t #f #f #t))
                          (list (list #t #t #t #f) (list #t #f #t #t) (list #f #f #f #f))
                          (list (list #f #t #t #f) (list #f #f #t #t) (list #f #f #f #f))))

(define pattern_ast (list (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                          (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                          (list (list #f #f #f #f) (list #t #t #t #t) (list #f #f #f #f))))

(define pattern_lbr (list (list (list #t #t #f #f) (list #f #f #f #t) (list #f #f #f #f))
                          (list (list #t #f #t #f) (list #f #f #f #f) (list #f #f #f #f))
                          (list (list #f #t #t #f) (list #f #f #f #t) (list #f #f #f #f))))

(define pattern_rbr (list (list (list #f #f #f #f) (list #f #t #f #f) (list #t #f #f #t))
                          (list (list #f #f #f #f) (list #f #f #f #f) (list #t #f #t #f))
                          (list (list #f #f #f #f) (list #f #t #f #f) (list #f #f #t #t))))

(define pattern_period (list (list (list #f #f #f #f) (list #t #t #f #f) (list #t #f #f #t))
                             (list (list #f #f #f #f) (list #f #t #t #f) (list #f #f #t #t))
                             (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_comma (list (list (list #f #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                            (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #t #t))
                            (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_apostrophe (list (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                                 (list (list #f #f #f #f) (list #f #f #f #f) (list #t #f #f #f))
                                 (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #t #t))))

(define pattern_quotes (list (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                             (list (list #f #f #f #f) (list #t #f #f #f) (list #t #f #f #f))
                             (list (list #f #f #f #f) (list #f #f #t #t) (list #f #f #t #t))))

(define pattern_colon (list (list (list #f #f #f #f) (list #t #t #f #f) (list #t #f #f #t))
                            (list (list #f #f #f #f) (list #t #t #t #f) (list #t #f #t #t))
                            (list (list #f #f #f #f) (list #f #t #t #f) (list #f #f #t #t))))

(define pattern_semicolon (list (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #t #t))
                                (list (list #f #f #f #f) (list #t #t #f #f) (list #t #f #f #t))
                                (list (list #f #f #f #f) (list #f #t #t #f) (list #f #f #t #t))))

(define pattern_plus (list (list (list #f #f #f #f) (list #t #f #f #f) (list #f #f #f #f))
                           (list (list #f #t #f #f) (list #t #t #t #t) (list #f #f #f #t))
                           (list (list #f #f #f #f) (list #f #f #t #f) (list #f #f #f #f))))

(define pattern_minus (list (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))
                            (list (list #f #t #f #f) (list #f #t #f #t) (list #f #f #f #t))
                            (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_slash (list (list (list #f #t #f #f) (list #t #f #f #t) (list #f #f #f #f))
                            (list (list #f #f #f #f) (list #f #t #t #f) (list #t #f #f #t))
                            (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #t #f))))

(define pattern_less (list (list (list #t #t #f #f) (list #f #f #f #t) (list #f #f #f #f))
                           (list (list #f #t #t #f) (list #f #f #f #t) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_greater (list (list (list #f #f #f #f) (list #f #t #f #f) (list #t #f #f #t))
                              (list (list #f #f #f #f) (list #f #t #f #f) (list #f #f #t #t))
                              (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_backslash (list (list (list #f #f #f #f) (list #t #t #f #f) (list #f #f #f #t))
                                (list (list #t #t #f #f) (list #f #f #t #t) (list #f #f #f #f))
                                (list (list #f #f #t #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_wlbr (list (list (list #f #f #f #f) (list #t #t #f #f) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #t #f #t #t) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #f #t #t #f) (list #f #f #f #f))))

(define pattern_wrbr (list (list (list #f #f #f #f) (list #t #f #f #t) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #t #t #t #f) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #f #f #t #t) (list #f #f #f #f))))

(define pattern_tilde (list (list (list #f #f #f #f) (list #t #t #f #f) (list #f #f #f #t))
                            (list (list #f #t #f #f) (list #f #f #t #t) (list #f #f #f #f))
                            (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f))))

(define pattern_pipe (list (list (list #f #f #f #f) (list #t #f #f #f) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #t #f #t #f) (list #f #f #f #f))
                           (list (list #f #f #f #f) (list #f #f #t #f) (list #f #f #f #f))))

(define pattern_question (list (list (list #f #f #f #f) (list #f #t #f #t) (list #f #f #f #f))
                               (list (list #f #f #f #f) (list #f #t #t #f) (list #t #f #f #t))
                               (list (list #f #t #f #f) (list #f #t #f #t) (list #f #f #t #t))))

(define (get-goal-lines letter)
  
  (cond
    ((string-ci=? letter "A") pattern_a)
    ((string-ci=? letter "B") pattern_b8)
    ((string-ci=? letter "C") pattern_c)
    ((string-ci=? letter "D") pattern_do0)
    ((string-ci=? letter "E") pattern_e)
    ((string-ci=? letter "F") pattern_f)
    ((string-ci=? letter "G") pattern_g)
    ((string-ci=? letter "H") pattern_h)
    ((string-ci=? letter "I") pattern_i)
    ((string-ci=? letter "J") pattern_j)
    ((string-ci=? letter "K") pattern_k)
    ((string-ci=? letter "L") pattern_l)
    ((string-ci=? letter "M") pattern_m)
    ((string-ci=? letter "N") pattern_n)
    ((string-ci=? letter "O") pattern_do0)
    ((string-ci=? letter "P") pattern_p)
    ((string-ci=? letter "Q") pattern_q)
    ((string-ci=? letter "R") pattern_r)
    ((string-ci=? letter "S") pattern_s5)
    ((string-ci=? letter "T") pattern_t)
    ((string-ci=? letter "U") pattern_uv)
    ((string-ci=? letter "V") pattern_uv)
    ((string-ci=? letter "W") pattern_w)
    ((string-ci=? letter "X") pattern_x)
    ((string-ci=? letter "Y") pattern_y)
    ((string=? letter "0") pattern_do0)
    ((string=? letter "1") pattern_1)
    ((string=? letter "2") pattern_2)
    ((string=? letter "3") pattern_3)
    ((string=? letter "4") pattern_4)
    ((string=? letter "5") pattern_s5)
    ((string=? letter "6") pattern_6)
    ((string=? letter "7") pattern_7)
    ((string=? letter "8") pattern_b8)
    ((string=? letter "9") pattern_9)
    ((string=? letter "!") pattern_exc)
    ((string=? letter "@") pattern_at)
    ((string=? letter "#") pattern_number)
    ((string=? letter "$") pattern_dollar)
    ((string=? letter "%") pattern_perc)
    ((string=? letter "^") pattern_hat)
    ((string=? letter "&") pattern_amp)
    ((string=? letter "*") pattern_ast)
    ((string=? letter "(") pattern_lbr)
    ((string=? letter ")") pattern_rbr)
    ((string=? letter "[") pattern_lbr)
    ((string=? letter "]") pattern_rbr)
    ((string=? letter ".") pattern_period)
    ((string=? letter ",") pattern_comma)
    ((string=? letter "'") pattern_apostrophe)
    ((string=? letter "\"") pattern_quotes)
    ((string=? letter ":") pattern_colon)
    ((string=? letter ";") pattern_semicolon)
    ((string=? letter "+") pattern_plus)
    ((string=? letter "-") pattern_minus)
    ((string=? letter "/") pattern_slash)
    ((string=? letter "<") pattern_less)
    ((string=? letter ">") pattern_greater)
    ((string=? letter "\\") pattern_backslash)
    ((string=? letter "{") pattern_wlbr)
    ((string=? letter "}") pattern_wrbr)
    ((string=? letter "~") pattern_tilde)
    ((string=? letter "|") pattern_pipe)
    (else pattern_question))
)


;; eof
