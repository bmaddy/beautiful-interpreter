(ns eval-expr
  (:require [clojure.test :as t :refer [is deftest testing run-tests]]
            [clojure.core.match :refer [match]]))

(defn environment [y]
  (if (symbol? y)
    (-> y resolve var-get)
    y))

(defn eval-expr
  ([expr] (eval-expr expr environment))
  ([expr env]
   (match [expr]
          [(x :guard symbol?)] (env x)
          [(['fn [x] body] :seq)] (fn [arg]
                                    (eval-expr body (fn [y]
                                                      (if (= x y)
                                                        arg
                                                        (env y)))))
          ;; needed for factorial - it should be possible to put this in the environment
          ;; but I haven't figured it out yet
          [(['if t a b] :seq)] (if (eval-expr t env)
                                 (eval-expr a env)
                                 (eval-expr b env))
          [([rator rand] :seq)] ((eval-expr rator env)
                                 (eval-expr rand env)))))

;; testing...

(deftest test-eval-expr
  (testing "number"
    (is (= 5
           (eval-expr 'five (fn [arg]
                              (case arg
                                'five 5
                                (environment arg)))))))

  (testing "symbol"
    (is (= 'hello
           (eval-expr 'hello
                      (fn [arg]
                        (if (= arg 'hello)
                          'hello
                          (environment arg)))))))

  (testing "use clojure.core as default environment"
    (is (= clojure.core/inc
           (eval-expr 'inc
                      (fn [arg]
                        (if (= arg 'hello)
                          'hello
                          (environment arg)))))))

  (testing "calling a function"
    (is (= 'hello
           (eval-expr '((fn [n] n) hello)
                      (fn [arg]
                        (if (= arg 'hello)
                          'hello
                          (environment arg)))))))

  (testing "scalar values"
    (is (= false
           (eval-expr (symbol "false") (fn [arg]
                                         (cond
                                           ;; need to do it this way because
                                           ;; (symbol? 'false) => false
                                           (and (symbol? arg) (= (name arg) "false")) false
                                           :else (environment arg)))))))

  (testing "'multi-arity' functions"
    (is (= 2
           (eval-expr '(inc one)
                      (fn [arg]
                        (case arg
                          'one 1
                          (environment arg))))))
    (is (= 6
           (eval-expr '((mult-by two) three)
                      (fn [arg]
                        (cond
                          (= arg 'two) 2
                          (= arg 'three) 3
                          (= arg 'mult-by) (fn [x] (partial * x))
                          :else (environment arg)))))))

  (testing "5! using the y-combinator"
    (let [fact-5 '(((fn [!]
                      (fn [n]
                        ((! !) n)))
                    (fn [!]
                      (fn [n]
                        (if (zero? n)
                          one
                          ((mult-by n) ((! !) (dec n)))))))
                   five)
          env (fn [arg]
                (cond
                  (= arg 'one) 1
                  (= arg 'five) 5
                  (= arg 'mult-by) (fn [x]
                                     (partial * x))
                  :else (environment arg)))]
      (is (= 120
             (eval-expr fact-5 env)))))

  )
