(ns rwn4clojure.core
  (:require [clojure.test :as t]))

(defn test-all []
  (t/run-tests *ns*))

(def all? (partial every? true?))


;; Write a function which creates a list of all integers in a given range.
(defn p34 [?hmm?]
  [(= (?hmm? 1 4) '(1 2 3))
   (= (?hmm? -2 2) '(-2 -1 0 1))
   (= (?hmm? 5 8) '(5 6 7))])
(t/deftest p34-test
  (t/testing "p34"
    (let [f (fn f [from upto]
              {:pre (<= from upto)}
              (loop [n from, acc []]
                (if (< n upto)
                  (recur (inc n) (conj acc n))
                  acc)))]
     (t/is (all? (p34 f))))))

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
      (t/is (all? (p134 f))))))

;; When retrieving values from a map, you can specify default values
;; in case the key is not found:
;;   (= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default
;; values? Write a function which takes a default value and a sequence
;; of keys and constructs a map.
(defn p156 [?hmm?]
  [(= (?hmm? 0 [:a :b :c]) {:a 0 :b 0 :c 0})
   (= (?hmm? "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
   (= (?hmm? [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})])
(t/deftest p156-test
  (t/testing "p156"
    (let [f (fn [v ks]
              (->> (for [k ks]
                     [k v])
                   (into {})))]
     (t/is (all? (p156 f))))))

;; Set A is a subset of set B, or equivalently B is a superset of A,
;; if A is "contained" inside B. A and B may coincide.
(defn p161 [?hmm?]
  [(clojure.set/superset? ?hmm? #{2})
   (clojure.set/subset? #{1} ?hmm?)
   (clojure.set/superset? ?hmm? #{1 2})
   (clojure.set/subset? #{1 2} ?hmm?)])
(t/deftest p161-test
  (t/testing "p161"
    (t/is (all? (p161 #{1 2})))))

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
    (t/is (all? (p162 1)))))
