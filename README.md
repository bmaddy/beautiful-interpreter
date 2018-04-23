# "the most beautiful program ever written"
as described here https://www.youtube.com/watch?v=OyfBQmvr2Hc in clojure


```bash
clj -i eval_expr.clj -e "(in-ns 'eval-expr)" -r
```

At the repl
```clojure
eval-expr=> (run-tests)

Testing eval-expr

FAIL in (test-eval-expr) (eval_expr.clj:72)
5! using the y-combinator
expected: (= 120 (eval-expr fact-5 environment))
  actual: (not (= 120 (* n ((! !) (dec n)))))

Ran 1 tests containing 7 assertions.
1 failures, 0 errors.
{:test 1, :pass 6, :fail 1, :error 0, :type :summary}
```
