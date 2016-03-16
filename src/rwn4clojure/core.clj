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

;; Write a function which, given a key and map, returns true
;; iff the map contains an entry with that key and its value is nil.
(defn p134 [?hmm?]
  [(true?  (?hmm? :a {:a nil :b 2}))
   (false? (?hmm? :b {:a nil :b 2}))
   (false? (?hmm? :c {:a nil :b 2}))])
(t/deftest p134-test
  (t/testing "p134"
    (let [f (fn [k m]
              (boolean (and (find m k)
                            (nil? (get m k)))))]
     (t/is (every? true? (p134 f))))))

;; In Clojure, only nil and false represent the values of logical
;; falsity in conditional tests - anything else is logical truth.
(defn p162 [?hmm?]
  [(= ?hmm? (if-not false 1 0))
   (= ?hmm? (if-not nil 1 0))
   (= ?hmm? (if true 1 0))
   (= ?hmm? (if [] 1 0))
   (= ?hmm? (if [0] 1 0))
   (= ?hmm? (if 0 1 0))
   (= ?hmm? (if 1 1 0))])
(t/deftest p162-test
  (t/testing "p162"
    (t/is ((partial every? true?) (p162 1)))))
