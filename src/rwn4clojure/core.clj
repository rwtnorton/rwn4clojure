(ns rwn4clojure.core
  (:require [clojure.test :as t]))

(defn test-all []
  (t/run-tests *ns*))

(defn p57 [?hmm?]
  (= ?hmm?
     ((fn foo [x]
        (when (> x 0)
          (conj (foo (dec x)) x)))
      5)))
(t/deftest p57-test
  (t/testing "p57"
    (t/is (p57 '(5 4 3 2 1)))))
