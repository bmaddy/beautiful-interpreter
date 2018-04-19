(ns eval-expr
  (:require [clojure.test :as t :refer [is deftest]]
            [clojure.core.match :refer [match]]))

(defn eval-expr
  [expr env]
  (match [expr]
         [(x :guard symbol?)] (env x)
         [(['fn [x] body] :seq)] (fn [arg]
                                   (eval-expr body (fn [y]
                                                     (if (= x y)
                                                       arg
                                                       (env y)))))
         [([rator rand] :seq)] ((eval-expr rator env)
                                (eval-expr rand env))))

(defn environment [y]
  (->> y name (symbol "clojure.core")))

(deftest test-eval-expr
  (is (= 'hello
         (eval-expr 'hello
                    (fn [arg]
                      (prn [:arg arg])
                      (if (= arg 'hello)
                        'hello
                        (environment arg))))))

  (is (= 'clojure.core/inc
         (eval-expr 'inc
                    (fn [arg]
                      (prn [:arg arg])
                      (if (= arg 'hello)
                        'hello
                        (environment arg))))))

  (is (= 'hello
         (eval-expr '((fn [n] n) hello)
                    (fn [arg]
                      (if (= arg 'hello)
                        'hello
                        (environment arg)))))))

