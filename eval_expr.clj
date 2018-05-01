(ns eval-expr
  (:require [clojure.test :as t :refer [is deftest testing run-tests]]
            [clojure.core.match :refer [match]]))

(defn environment [y]
  (->> y name (symbol "clojure.core") find-var var-get))

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
          [(['if t a b] :seq)] (if (eval-expr t env)
                                 (eval-expr a env)
                                 (eval-expr b env))
          [([rator rand] :seq)] ((eval-expr rator env)
                                 (eval-expr rand env))
          :else expr)))

;; testing...

(deftest test-eval-expr
  (testing "number"
    (is (= 5
           (eval-expr '5 environment))))

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
           (eval-expr 'false environment))))

  (testing "special forms"
    (is (= false
           (eval-expr 'false environment))))

  (testing "2-arity functions"
    (is (= 2
           (eval-expr '(inc 1))))
    (is (= 6
           (eval-expr '((mult-by 2) 3)
                      (fn [arg]
                        (if (= arg 'mult-by)
                          (fn [x]
                            (partial * x))
                          (environment arg)))))))

  (testing "5! using the y-combinator"
    (let [fact-5 '(((fn [!]
                      (fn [n]
                        ((! !) n)))
                    (fn [!]
                      (fn [n]
                        (if (zero? n)
                          1
                          ((mult-by n) ((! !) (dec n)))))))
                   5)
          env (fn [arg]
                (if (= arg 'mult-by)
                  (fn [x]
                    (partial * x))
                  (environment arg)))]
      (is (= 120
             (eval-expr fact-5 env)))))

  )
