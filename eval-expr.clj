  (defn eval-expr
    [expr env]
    ;; (prn expr)
    ;; (prn [expr (symbol? expr) 'fn (symbol? 'fn)])
    ;; (prn (get env expr))
    (cond
        (symbol? expr) (do (prn :get)
                           (env expr))
        (= 'fn (first expr)) (do (let [[_ [x] body] expr]
                                   (prn [:fn x body])
                                   (fn [arg]
                                     (eval-expr body (fn [y]
                                                       (prn {:x x :y y})
                                                       (if (= x y)
                                                         arg
                                                         (get env y)))))))
        :else (do (prn :else)
                  (let [[rator rand] expr]
                    ((eval-expr rator env)
                     (eval-expr rand env))))))

  (defn environment [y]
    (throw (Exception. "oops")))

  (eval-expr 'hello
             (fn [arg]
               (prn [:arg arg])
               (if (= arg 'hello)
                 'hello
                 (environment arg))))

  (eval-expr '((fn (n) n) hello)
             (fn [arg]
               (if (= arg 'hello)
                 'hello
                 (environment arg))))

