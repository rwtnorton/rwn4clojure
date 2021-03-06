(ns rwn4clojure.core
  (:require [clojure.test :refer :all]))

(defn test-all []
  (run-tests *ns*))

(def all? (partial every? true?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p1 [__]
  (= __ true))
(deftest t1
  (is (p1 true)))

(defn p2 [__]
  (= (- 10 (* 2 3)) __))
(deftest t2
  (is (p2 4)))

(defn p3 [__]
  (= __ (.toUpperCase "hello world")))
(deftest t3
  (is (p3 "HELLO WORLD")))

(defmacro p4 [& __]
  `(= (list ~@__) '(:a :b :c)))
(deftest t4
  (is (p4 :a :b :c)))

(defn p5 [__]
  [(= __ (conj '(2 3 4) 1))
   (= __ (conj '(3 4) 2 1))])
(deftest t5
  (is (all? (p5 '(1 2 3 4)))))

(defmacro p6 [& __]
  `(= [~@__] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))
(deftest t6
  (is (p6 :a :b :c)))

(defn p7 [__]
  [(= __ (conj [1 2 3] 4))
   (= __ (conj [1 2] 3 4))])
(deftest t7
  (is (p7 [1 2 3 4])))

(defn p8 [__]
  [(= __ (set '(:a :a :b :c :c :c :c :d :d)))
   (= __ (clojure.set/union #{:a :b :c} #{:b :c :d}))])
(deftest t8
  (is (p8 #{:a :b :c :d})))

(defn p9 [__]
  (= #{1 2 3 4} (conj #{1 4 3} __)))
(deftest t9
  (is (p9 2)))

(defn p10 [__]
  [(= __ ((hash-map :a 10, :b 20, :c 30) :b))
   (= __ (:b {:a 10, :b 20, :c 30}))])
(deftest t10
  (is (p10 20)))

(defn p11 [__]
  (= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3])))
(deftest t11
  (is (p11 [:b 2])))

(defn p12 [__]
  [(= __ (first '(3 2 1)))
   (= __ (second [2 3 4]))
   (= __ (last (list 1 2 3)))])
(deftest t12
  (is (all? (p12 3))))

(defn p13 [__]
  (= __ (rest [10 20 30 40])))
(deftest t13
  (is (p13 '(20 30 40))))

(defn p14 [__]
  [(= __ ((fn add-five [x] (+ x 5)) 3))
   (= __ ((fn [x] (+ x 5)) 3))
   (= __ (#(+ % 5) 3))
   (= __ ((partial + 5) 3))])
(deftest t14
  (is (all? (p14 8))))

(defn p15 [__]
  [(= (__ 2) 4)
   (= (__ 3) 6)
   (= (__ 11) 22)
   (= (__ 7) 14)])
(deftest t15
  (is (all? (p15 (partial * 2)))))

(defn p16 [__]
  [(= (__ "Dave") "Hello, Dave!")
   (= (__ "Jenn") "Hello, Jenn!")
   (= (__ "Rhea") "Hello, Rhea!")])
(deftest t16
  (is (all? (p16 #(str "Hello, " % "!")))))

(defn p17 [__]
  (= __ (map #(+ % 5) '(1 2 3))))
(deftest t17
  (is (p17 '(6 7 8))))

(defn p18 [__]
  (= __ (filter #(> % 5) '(3 4 5 6 7))))
(deftest t18
  (is (p18 [6 7])))

(defn p19 [__]
  [(= (__ [1 2 3 4 5]) 5)
   (= (__ '(5 4 3)) 3)
   (= (__ ["b" "c" "d"]) "d")])
(deftest t19
  (let [f (fn [xs]
            (cond
              (empty? xs) nil
              (empty? (rest xs)) (first xs)
              :else (recur (rest xs))))]
    (is (all? (p19 f)))))

(defn p20 [__]
  [(= (__ (list 1 2 3 4 5)) 4)
   (= (__ ["a" "b" "c"]) "b")
   (= (__ [[1 2] [3 4]]) [1 2])])
(deftest t20
  (let [old-f (fn [xs]
                (cond
                  (empty? xs) nil
                  (empty? (rest xs)) nil
                  (empty? (rest (rest xs))) (first xs)
                  :else (recur (rest xs))))
        f (fn [xs] (->> xs reverse rest first))]
    (is (all? (p20 f)))))

(defn p21 [__]
  [(= (__ '(4 5 6 7) 2) 6)
   (= (__ [:a :b :c] 0) :a)
   (= (__ [1 2 3 4] 1) 2)
   (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])])
(deftest t21
  (let [f (fn [xs n]
            (if (zero? n)
              (first xs)
              (recur (rest xs) (dec n))))]
    (is (all? (p21 f)))))

(defn p22 [__]
  [(= (__ '(1 2 3 3 1)) 5)
   (= (__ "Hello World") 11)
   (= (__ [[1 2] [3 4] [5 6]]) 3)
   (= (__ '(13)) 1)
   (= (__ '(:a :b :c)) 3)])
(deftest t22
  (let [f (fn [xs]
            (let [lp (fn [xs' n]
                       (if (empty? xs')
                         n
                         (recur (rest xs') (inc n))))]
              (lp xs 0)))]
    (is (all? (p22 f)))))

(defn p23 [__]
  [(= (__ [1 2 3 4 5]) [5 4 3 2 1])
   (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
   (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])])
(deftest t23
  (let [f (fn [xs]
            (reduce conj '() xs))]
   (is (all? (p23 f)))))

(defn p24 [__]
  [(= (__ [1 2 3]) 6)
   (= (__ (list 0 -2 5 5)) 8)
   (= (__ #{4 2 1}) 7)
   (= (__ '(0 0 -1)) -1)
   (= (__ '(1 10 3)) 14)])
(deftest t24
  (let [f (partial apply +)]
    (is (all? (p24 f)))))

(defn p25 [__]
  [(= (__ #{1 2 3 4 5}) '(1 3 5))
   (= (__ [4 2 1 6]) '(1))
   (= (__ [2 2 4 6]) '())
   (= (__ [1 1 1 3]) '(1 1 1 3))])
(deftest t25
  (is (all? (p25 (partial filter odd?)))))

(defn p26 [__]
  [(= (__ 3) '(1 1 2))
   (= (__ 6) '(1 1 2 3 5 8))
   (= (__ 8) '(1 1 2 3 5 8 13 21))])
(deftest t26
  (let [old-f (fn [n]
                (take n ((fn lp [a b]
                           (cons a (lazy-seq (lp b (+ a b))))) 1 1)))
        f (fn [n]
            (->> (iterate (fn [[a b]] [b (+ a b)]) [0 1])
                 (take n)
                 (map last)))]
   (is (all? (p26 f)))))

(defn p27 [__]
  [(false? (__ '(1 2 3 4 5)))
   (true? (__ "racecar"))
   (true? (__ [:foo :bar :foo]))
   (true? (__ '(1 1 3 3 1 1)))
   (false? (__ '(:a :b :c)))])
(deftest t27
  (let [old-f (fn [vs]
                (let [vs (seq vs)]
                  (= vs (reverse vs))))
        f (fn [vs]
            (let [n (quot (count vs) 2)
                  front-half (take n vs)
                  back-half (->> vs reverse (take n))]
              (= front-half back-half)))]
    (is (all? (p27 f)))))

(defn p28 [__]
  [(= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
   (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
   (= (__ '((((:a))))) '(:a))])
(deftest t28
  (let [f (fn [form]
            (let [todo (atom [form])
                  result (atom [])]
              (while (seq @todo)
                (let [curr (peek @todo)]
                  (swap! todo pop)
                  (if (sequential? curr)
                    (doseq [kid (reverse curr)]
                      (swap! todo conj kid))
                    (swap! result conj curr))))
              @result))
        #_(fn [form]
            (->> (clojure.zip/zipper sequential? seq (fn [_ c] c) form)
                 (iterate clojure.zip/next)
                 (take-while (complement clojure.zip/end?))
                 (map clojure.zip/node)
                 (remove sequential?)))
        #_(fn f [form]
            (loop [curr form, todo [], acc []]
              (if (sequential? curr)
                (apply conj acc (map f curr))
                #_(if (seq curr)
                  (recur (rest curr) (conj acc (first curr)))
                  (recur (rest curr) acc))
                (conj acc curr))))]
    (is (all? (p28 f)))))

(defn p29 [__]
  [(= (__ "HeLlO, WoRlD!") "HLOWRD")
   (empty? (__ "nothing"))
   (= (__ "$#A(*&987Zf") "AZ")])
(deftest t29
  (let [f (fn [s]
            (->> s
                 (re-seq #"[A-Z]+")
                 (clojure.string/join "")))]
    (is (all? (p29 f)))))

(defn p30 [__]
  [(= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
   (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
   (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))])
(deftest t30
  (let [f (fn [vs]
            (reduce (fn [acc v]
                      (cond
                        (empty? acc) (conj (empty acc) v)
                        (= (last acc) v) acc
                        :else (conj acc v)))
                    []
                    vs))]
    (is (all? (p30 f)))))

(defn p31 [__]
  [(= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
   (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
   (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))])
(deftest t31
  (let [f (fn [vs]
            (reduce (fn [acc v]
                      (cond
                        (empty? acc) (conj acc (vector v))
                        (= (-> acc last first) v) (update-in acc [(-> acc count dec)] conj v)
                        :else (conj acc (vector v))))
                    []
                    vs))]
    (is (all? (p31 f)))))

(defn p32 [__]
  [(= (__ [1 2 3]) '(1 1 2 2 3 3))
   (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
   (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
   (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))])
(deftest t32
  (let [f (fn [vs]
            (interleave vs vs))]
    (is (all? (p32 f)))))

(defn p33 [__]
  [(= (__ [1 2 3] 2) '(1 1 2 2 3 3))
   (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
   (= (__ [4 5 6] 1) '(4 5 6))
   (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
   (= (__ [44 33] 2) [44 44 33 33])])
(deftest t33
  (let [f (fn [vs n]
            (if (= 1 n)
              vs
              (apply interleave (repeat n vs))))]
    (is (all? (p33 f)))))

;; Write a function which creates a list of all integers in a given range.
(defn p34 [__]
  [(= (__ 1 4) '(1 2 3))
   (= (__ -2 2) '(-2 -1 0 1))
   (= (__ 5 8) '(5 6 7))])
(deftest t34
  (let [f (fn f [from upto]
            {:pre (<= from upto)}
            (loop [n from, acc []]
              (if (< n upto)
                (recur (inc n) (conj acc n))
                acc)))]
    (is (all? (p34 f)))))

(defn p35 [__]
  [(= __ (let [x 5] (+ 2 x)))
   (= __ (let [x 3, y 10] (- y x)))
   (= __ (let [x 21] (let [y 3] (/ x y))))])
(deftest t35
  (is (all? (p35 7))))

(defn p36 [__]
  [(= 10 (let [x 7 y 3 z 1] (+ x y)))
   (= 4 (let [x 7 y 3 z 1] (+ y z)))
   (= 1 (let [x 7 y 3 z 1] z))])
(deftest t36
  (declare x y z) ;; hacks
  (is (all? (p36 [x 7
                  y 3
                  z 1]))))

(defn p37 [__]
  (= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))
(deftest t37
  (is (p37 "ABC")))

(defn p38 [__]
  [(= (__ 1 8 3 4) 8)
   (= (__ 30 20) 30)
   (= (__ 45 67 11) 67)])
(deftest t38
  (let [f (fn [& vs] (reduce (fn [x y] (if (> x y) x y)) vs))]
    (is (all? (p38 f)))))

(defn p39 [__]
  [(= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
   (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
   (= (__ [1 2 3 4] [5]) [1 5])
   (= (__ [30 20] [25 15]) [30 25 20 15])])
(deftest t39
  (let [f (fn [xs ys] (flatten (map vector xs ys)))]
    (is (all? (p39 f)))))

(defn p40 [__]
  [(= (__ 0 [1 2 3]) [1 0 2 0 3])
   (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
   (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])])
(deftest t40
  (let [f (fn [tween xs]
            (let [hmm (fn hmm [sep xs acc]
                        (if (seq xs)
                          (let [[x & etc] xs
                                acc' (if (seq etc)
                                       (conj acc x sep)
                                       (conj acc x))]
                            (recur sep etc acc'))
                          acc))]
              (hmm tween xs [])))]
    (is (all? (p40 f)))))

(defn p41 [__]
  [(= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
   (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
   (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])])
(deftest t41
  (let [f (fn [vs n]
            (->> vs
                 (map-indexed (fn [i v] [(inc i) v]))
                 (remove (fn [[i v]] (-> i (rem n) zero?)))
                 (map last)))]
    (is (all? (p41 f)))))

(defn p42 [__]
  [(= (__ 1) 1)
   (= (__ 3) 6)
   (= (__ 5) 120)
   (= (__ 8) 40320)])
(deftest t42
  (let [old-f (fn [n]
                (loop [acc 1, i n]
                  (if (<= i 1)
                    acc
                    (recur (* acc i) (dec i)))))
        f (fn [n]
            (->> (range n 0 -1)
                 (apply *)))]
    (is (all? (p42 f)))))

(defn p43 [__]
  [(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
   (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
   (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))])
(deftest t43
  (let [f (fn [vs n]
            (let [ith-mod-pair (fn [target] (fn [[i _]] (= target (mod i n))))
                  enumerate (fn [xs] (map-indexed (fn [i x] [i x]) xs))
                  ith-seq (fn [target] (->> vs
                                            enumerate
                                            (filter (ith-mod-pair target))
                                            (map last)))]
              (->> n
                   range
                   (map ith-seq))))]
    (is (all? (p43 f)))))

(defn p44 [__]
  [(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
   (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
   (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
   (= (__ 1 '(:a :b :c)) '(:b :c :a))
   (= (__ -4 '(:a :b :c)) '(:c :a :b))])
(deftest t44
  (let [old-f (fn rot [n vs]
                (let [m (mod n (count vs))
                      [front back] (split-at m vs)]
                  (into (vec back) front)))
        f (fn rot [n vs]
            (let [m (mod n (count vs))]
              (apply concat (reverse (split-at m vs)))))]
    (is (all? (p44 f)))))

(defn p45 [__]
  (= __ (take 5 (iterate #(+ 3 %) 1))))
(deftest t45
  (is (p45 [1 4 7 10 13])))

(defn p46 [__]
  [(= 3 ((__ nth) 2 [1 2 3 4 5]))
   (= true ((__ >) 7 8))
   (= 4 ((__ quot) 2 8))
   (= [1 2 3] ((__ take) [1 2 3 4 5] 3))])
(deftest t46
  (let [f (fn [f]
            (fn [a b] (f b a)))]
    (is (all? (p46 f)))))

(defn p47 [__]
  [(contains? #{4 5 6} __)
   (contains? [1 1 1 1 1] __)
   (contains? {4 :a 2 :b} __)
   (not (contains? [1 2 4] __))])
(deftest t47
  (is (all? (p47 4))))

(defn p48 [__]
  [(= __ (some #{2 7 6} [5 6 7 8]))
   (= __ (some #(when (even? %) %) [5 6 7 8]))])
(deftest t48
  (is (all? (p48 6))))

(defn p49 [__]
  [(= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
   (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
   (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])])
(deftest t49
  (let [f (fn f [n vs]
            (loop [n n, hd [], tl (seq vs)]
              (if (zero? n)
                [hd tl]
                (recur (dec n)
                       (conj hd (first tl))
                       (rest tl)))))]
    (is (all? (p49 f)))))

(defn p50 [__]
  [(= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
   (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
   (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})])
(deftest t50
  (let [f (fn [vs] (->> vs (group-by class) vals))]
    (is (all? (p50 f)))))

(defn p51 [__]
  (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d])))
(deftest t51
  (is (p51 [1 2 3 4 5])))

(defn p52 [__]
  (= [2 4] (let [[a b c d e] [0 1 2 3 4]] #___ [c e])))
(deftest t52
  (declare c e) ;; hacks
  (is (p52 [c e])))

;; Write a function which returns a sequence of lists of x items each.
;; Lists of less than x items should not be returned.
(defn p54 [__]
  [(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
   (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
   (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))])
(deftest t54
  (let [f (fn [n vs]
            (letfn [(vec->list [v] (->> v rseq (into '())))]
              (loop [vs' vs, result [], bucket []]
                (if (seq vs')
                  (if (and (empty? bucket)
                           (< (count vs') n))
                    (vec->list result)
                    (let [v (first vs')
                          new-bucket (conj bucket v)]
                      (if (= n (count new-bucket))
                        (recur (rest vs')
                               (conj result (vec->list new-bucket))
                               [])
                        (recur (rest vs') result new-bucket))))
                  (vec->list result)))))]
    (is (all? (p54 f)))))

(defn p55 [__]
  [(= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
   (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
   (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})])
(deftest t55
  (let [f (fn [vs]
            (->> vs
                 (group-by (set vs))
                 (map (fn [[k v]]
                        [k (count v)]))
                 (into {})))]
    (is (all? (p55 f)))))

;; Write a function which removes the duplicates from a sequence. Order
;; of the items must be maintained.
(defn p56 [__]
  [(= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
   (= (__ [:a :a :b :b :c :c]) [:a :b :c])
   (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
   (= (__ (range 50)) (range 50))])
(deftest t56
  (let [f (fn [vs]
            (loop [vs' vs, seen #{}, acc []]
              (if (seq vs')
                (let [v (first vs')]
                  (if (seen v)
                    (recur (rest vs') seen acc)
                    (recur (rest vs') (conj seen v) (conj acc v))))
                acc)))
        g (fn [vs]
            (reduce (fn [acc v]
                      (let [seen (set acc)]
                        (if (seen v)
                          acc
                          (conj acc v))))
                    []
                    vs))]
    (is (all? (p56 g)))))

(defn p57 [__]
  (= __
     ((fn foo [x]
        (when (> x 0)
          (conj (foo (dec x)) x)))
      5)))
(deftest t57
  (is (p57 '(5 4 3 2 1))))

;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and
;; create a function that applies them from right-to-left.
(defn p58 [__]
  [(= [3 2 1] ((__ rest reverse) [1 2 3 4]))
   (= 5 ((__ (partial + 3) second) [1 2 3 4]))
   (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
   (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))])
(deftest t58
  (let [f (fn [& fns]
            (fn [& xs]
              (loop [vs xs, gs (reverse fns)]
                (if (seq gs)
                  (let [g (first gs)
                        vs' (list (apply g vs))]
                    (recur vs' (rest gs)))
                  (first vs)))))
        h (fn [& fns]
            (fn [& xs]
              (->> (reduce (fn [acc f]
                             (list (apply f acc)))
                           xs
                           (reverse fns))
                   first)))]
    (is (all? (p58 h)))))

;; Take a set of functions and return a new function that takes a
;; variable number of arguments and returns a sequence containing the
;; result of applying each function left-to-right to the argument list.
(defn p59 [__]
  [(= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
   (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
   (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))])
(deftest t59
  (let [f (fn [& fns]
            (fn [& vs]
              (for [g fns]
                (apply g vs))))]
    (is (all? (p59 f)))))

;; Write a function which behaves like reduce, but returns each
;; intermediate value of the reduction. Your function must accept
;; either two or three arguments, and the return sequence must be lazy.
(defn p60 [__]
  [(= (take 5 (__ + (range))) [0 1 3 6 10])
   (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
   (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)])
(deftest t60
  (let [f (fn f
            ([g vs]
             (f g (first vs) (rest vs)))
            ([g ini vs]
             (let [foo (fn [[x xs]]
                         (if (seq xs)
                           [(g x (first xs))
                            (rest xs)]
                           []))]
               (->> [ini vs]
                    (iterate foo)
                    (take-while seq)
                    (map first)))))]
    (is (all? (p60 f)))))

(defn p61 [__]
  [(= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
   (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
   (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})])
(deftest t61
  (let [f (fn f [ks vs]
            (->> (map vector ks vs)
                 (into {})))]
    (is (all? (p61 f)))))

(defn p62 [__]
  [(= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
   (= (take 100 (__ inc 0)) (take 100 (range)))
   (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))])
(deftest t62
  (let [f (fn f [g x]
            (lazy-seq (cons x (f g (g x)))))]
    (is (all? (p62 f)))))

(defn p63 [__]
  [(= (__ #(> % 5) [1 3 6 8])
      {false [1 3], true [6 8]})
   (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
      {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
   (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
      {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})])
(deftest t63
  (let [f (fn [g vs]
            (reduce
             (fn [acc v]
               (update-in acc [(g v)] (fnil conj []) v))
             {}
             vs))]
    (is (all? (p63 f)))))

(defn p64 [__]
  [(= 15 (reduce __ [1 2 3 4 5]))
   (=  0 (reduce __ []))
   (=  6 (reduce __ 1 [2 3]))])
(deftest t64
  (is (all? (p64 +))))

;; Clojure has many sequence types, which act in subtly different ways.
;; The core functions typically convert them into a uniform "sequence"
;; type and work with them that way, but it can be important to
;; understand the behavioral and performance differences so that you
;; know which kind is appropriate for your application.

;; Write a function which takes a collection and returns one of :map,
;; :set, :list, or :vector - describing the type of collection it was
;; given.
;; You won't be allowed to inspect their class or use the built-in
;; predicates like list? - the point is to poke at them and understand
;; their behavior.
(defn p65 [__]
  [(= :map (__ {:a 1, :b 2}))
   (= :list (__ (range (rand-int 20))))
   (= :vector (__ [1 2 3 4 5 6]))
   (= :set (__ #{10 (rand-int 5)}))
   (= [:map :set :vector :list] (map __ [{} #{} [] ()]))])
(deftest t65
  (let [f (fn [coll]
            (case (->> coll empty str)
              "{}" :map
              "#{}" :set
              "[]" :vector
              "()" :list
              :dunno))
        g (fn [coll]
            (let [z (empty coll)]
              (cond
                (= 1 (count (conj z [1 1] [1 2])))
                :map

                (= 1 (count (conj z [1 1] [1 1])))
                :set

                (= [1 1] (first (conj z [1 1] [2 2])))
                :vector

                (= [2 2] (first (conj z [1 1] [2 2])))
                :list

                :else :dunno)))]
    (is (all? (p65 f)))))

(defn p66 [__]
  [(= (__ 2 4) 2)
   (= (__ 10 5) 5)
   (= (__ 5 7) 1)
   (= (__ 1023 858) 33)])
(deftest t66
  (let [f (fn [& nums]
            (let [[big lil] (sort > nums)]
              (loop [a big
                     b lil]
                (let [q (quot a b)
                      r (rem a b)]
                  (if (zero? r)
                    b
                    (recur b r))))))]
    (is (all? (p66 f)))))

(defn p67 [__]
  [(= (__ 2) [2 3])
   (= (__ 5) [2 3 5 7 11])
   (= (last (__ 100)) 541)])
(deftest t67
  (let [f (fn [num-primes]
            (let [prime?
                  (fn [n]
                    (let [m (->> n Math/sqrt long)
                          multiple? (fn [x]
                                      (zero? (mod n x)))]
                      (if (< n 2)
                        false
                        (->> (range 2 (inc m))
                             (not-any? multiple?)))))]
              (take num-primes (filter prime? (range)))))]
    (is (all? (p67 f)))))

(defn p68 [__]
  (= __
  (loop [x 5
         result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result))))
(deftest t68
  (is (p68 [7 6 5 4 3])))

;; Write a function which takes a function f and a variable number of
;; maps. Your function should return a map that consists of the rest
;; of the maps conj-ed onto the first. If a key occurs in more than
;; one map, the mapping(s) from the latter (left-to-right) should be
;; combined with the mapping in the result by calling (f val-in-result
;; val-in-latter)
(defn p69 [__]
  [(= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
      {:a 4, :b 6, :c 20})
   (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
      {1 7, 2 10, 3 15})
   (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
      {:a [3 4 5], :b [6 7], :c [8 9]})])
(deftest t69
  (let [f (fn [g & ms]
            (let [smoosh (fn [m1 m2]
                           (let [ks (->> [m1 m2]
                                         (map (comp set keys))
                                         (apply clojure.set/union))]
                             (->> (for [k ks]
                                    (let [vs (->> [m1 m2]
                                                  (map (fn [m] (get m k)))
                                                  (filter identity))]
                                      [k (if (== 1 (count vs))
                                           (first vs)
                                           (apply g vs))]))
                                  (into {}))))]
              (reduce smoosh ms)))]
    (is (all? (p69 f)))))

;; Write a function that splits a sentence up into a sorted list of
;; words. Capitalization should not affect sort order and punctuation
;; should be ignored.
(defn p70 [__]
  [(= (__  "Have a nice day.")
      ["a" "day" "Have" "nice"])
   (= (__  "Clojure is a fun language!")
      ["a" "Clojure" "fun" "is" "language"])
   (= (__  "Fools fall for foolish follies.")
      ["fall" "follies" "foolish" "Fools" "for"])])
(deftest t70
  (let [f (fn [s]
            (->> (clojure.string/split s #"\W+")
                 (sort-by clojure.string/lower-case)))]
    (is (all? (p70 f)))))

(defn p71 [__]
  (= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__))
   5))
(deftest t71
  (is (p71 last)))

(defmacro p72 [& __]
  `(= (~@__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (~@__))
   11))
(deftest t72
  (is (p72 apply +)))

;; Given a string of comma separated integers, write a function which
;; returns a new comma separated string that only contains the numbers
;; which are perfect squares.
(defn p74 [__]
  [(= (__ "4,5,6,7,8,9") "4,9")
   (= (__ "15,16,25,36,37") "16,25,36")])
(deftest t74
  (let [f (fn [s]
            (let [parse-int (fn [v] (Integer/parseInt v))
                  perfect-square? (fn [n]
                                    (let [r (int (Math/sqrt n))]
                                      (= n (* r r))))]
              (->> (clojure.string/split s #",\s*")
                   (map parse-int)
                   (filter perfect-square?)
                   (clojure.string/join ","))))]
    (is (all? (p74 f)))))

;; Two numbers are coprime if their greatest common divisor equals 1.
;; Euler's totient function f(x) is defined as the number of positive
;; integers less than x which are coprime to x. The special case f(1)
;; equals 1. Write a function which calculates Euler's totient function.
(defn p75 [__]
  [(= (__ 1) 1)
   (= (__ 10) (count '(1 3 7 9)) 4)
   (= (__ 40) 16)
   (= (__ 99) 60)])
(deftest t75
 (let [gcd (fn [& nums]
             (let [[big lil] (sort > nums)]
               (loop [a big
                      b lil]
                 (let [q (quot a b)
                       r (rem a b)]
                   (if (zero? r)
                     b
                     (recur b r))))))
       coprime? (fn [n1 n2]
                  (== 1 (gcd n1 n2)))
       f (fn [n]
           {:pre [(pos? n)]}
           (if (== 1 n)
             1
             (->> (range 1 n)
                  (filter (partial coprime? n))
                  count)))]
   (is (all? (p75 f)))))

;; The trampoline function takes a function f and a variable number
;; of parameters. Trampoline calls f with any parameters that were
;; supplied. If f returns a function, trampoline calls that function
;; with no arguments. This is repeated, until the return value is not
;; a function, and then trampoline returns that non-function value.
;; This is useful for implementing mutually recursive algorithms in a
;; way that won't consume the stack.
(defn p76 [__]
  (= __
     (letfn
         [(foo [x y] #(bar (conj x y) y))
          (bar [x y] (if (> (last x) 10)
                       x
                       #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))
(deftest t76
  (let [f (fn [g & vs]
            (let [r (apply g vs)]
              (if (fn? r)
               (loop [g' r]
                 (let [h (g')]
                   (if (fn? h)
                     (recur h)
                     h)))
               r)))]
    (is (p76 [1 3 5 7 9 11]))))

(defn p77 [__]
  [(= (__ ["meat" "mat" "team" "mate" "eat"])
      #{#{"meat" "team" "mate"}})
   (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
      #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})])
(deftest t77
  (let [f (fn [strs]
            (->> strs
                 (group-by frequencies)
                 vals
                 (filter (fn [vs] (> (count vs) 1)))
                 (map set)
                 set))]
    (is (all? (p77 f)))))

;; Reimplement the function described in "Intro to Trampoline".
(defn p78 [__]
  [(= (letfn [(triple [x] #(sub-two (* 3 x)))
              (sub-two [x] #(stop?(- x 2)))
              (stop? [x] (if (> x 50) x #(triple x)))]
        (__ triple 2))
      82)
   (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
              (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
        (map (partial __ my-even?) (range 6)))
      [true false true false true false])])
(deftest t78
  (let [f (fn [g & vs]
            (let [r (apply g vs)]
              (if (fn? r)
               (loop [g' r]
                 (let [h (g')]
                   (if (fn? h)
                     (recur h)
                     h)))
               r)))]
    (is (all? (p78 f)))))

;; A number is "perfect" if the sum of its divisors equal the number
;; itself. 6 is a perfect number because 1+2+3=6. Write a function
;; which returns true for perfect numbers and false otherwise.
(defn p80 [__]
  [(= (__ 6) true)
   (= (__ 7) false)
   (= (__ 496) true)
   (= (__ 500) false)
   (= (__ 8128) true)])
(deftest t80
  (let [f (fn [n]
            (->> n
                 Math/sqrt
                 int
                 range
                 (map inc)
                 (filter (fn [d] (zero? (rem n d))) )
                 (map (fn [v] [v (quot n v)]))
                 rest
                 flatten
                 (into #{})
                 (apply +)
                 inc
                 (= n)))]
    (is (all? (p80 f)))))

(defn p81 [__]
  [(= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
   (= (__ #{0 1 2} #{3 4 5}) #{})
   (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})])
(deftest t81
  (let [f (fn [set1 set2]
            (reduce (fn [acc v]
                      (if (set2 v)
                        (conj acc v)
                        acc))
                    #{}
                    set1)
            #_(loop [acc #{}, noms set1, over set2]
              (if (seq noms)
                (let [v (first noms)
                      nomd (disj noms v)]
                  (recur (if (over v)
                           (conj acc v)
                           acc)
                         nomd
                         over))
                acc)))]
    (is (all? (p81 f)))))

(defn p83 [__]
  [(= false (__ false false))
   (= true (__ true false))
   (= false (__ true))
   (= true (__ false true false))
   (= false (__ true true true))
   (= true (__ true true true false))])
(deftest t83
  (let [f (fn [& bs]
            (and (-> (some identity bs) not not)
                 (not-every? identity bs)))]
    (is (all? (p83 f)))))

;; Write a function which generates the power set of a given set. The
;; power set of a set x is the set of all subsets of x, including the
;; empty set and x itself.
(defn p85 [__]
  [(= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
   (= (__ #{}) #{#{}})
   (= (__ #{1 2 3})
      #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
   (= (count (__ (into #{} (range 10)))) 1024)])
(deftest t85
  (let [bits->indices (fn [n]
                        (loop [x n, acc [], i 0]
                          (if (zero? x)
                            acc
                            (let [q (quot x 2)
                                  r (rem x 2)
                                  acc' (if (= 1 r) (conj acc i) acc)]
                              (recur q acc' (inc i))))))
        f (fn [u]
            (let [n (apply *' (repeat (count u) 2))
                  v (vec u)]
              (->> (for [b (range n)]
                     (let [idxs (bits->indices b)]
                       (->> (map (partial nth v) idxs)
                            (into #{}))))
                   (into #{}))))]
    (is (all? (p85 f)))))

;; Happy numbers are positive integers that follow a particular formula:
;; take each individual digit, square it, and then sum the squares to
;; get a new number. Repeat with the new number and eventually, you
;; might get to a number whose squared sum is 1. This is a happy number.
;; An unhappy number (or sad number) is one that loops endlessly. Write
;; a function that determines if a number is happy or not.
(defn p86 [__]
  [(= (__ 7) true)
   (= (__ 986543210) true)
   (= (__ 2) false)
   (= (__ 3) false)])
(deftest t86
  (let [->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                        (take-while (fn [vs] (some (complement zero?) vs)))
                        (map second)
                        rest
                        reverse
                        (into [])))
        sumsq-of-digits (fn [n]
                          (->> n
                               ->digits
                               (map (fn [x] (*' x x)))
                               (apply +')))
        f (fn [n]
            (loop [x n, seen #{}]
              (if (seen x)
                false
                (let [x' (sumsq-of-digits x)]
                  (if (== 1 x')
                    true
                    (recur x' (conj seen x)))))))]
    (is (all? (p86 f)))))

(defn p88 [__]
  [(= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
   (= (__ #{:a :b :c} #{}) #{:a :b :c})
   (= (__ #{} #{4 5 6}) #{4 5 6})
   (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})])
(deftest t88
  (let [f (fn [a b]
            (reduce (fn [acc v]
                      (if (not (and (a v) (b v)))
                        (conj acc v)
                        acc))
                    #{}
                    (clojure.set/union a b)))]
    (is (all? (p88 f)))))

(defn p90 [__]
  [(= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
   (= (__ #{1 2 3} #{4 5})
      #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
   (= 300 (count (__ (into #{} (range 10))
                     (into #{} (range 30)))))])
(deftest t90
  (let [f (fn [xs ys]
            (set
             (for [x xs
                   y ys]
               [x y])))]
    (is (all? (p90 f)))))

;; Write a function which flattens any nested combination of sequential
;; things (lists, vectors, etc.), but maintains the lowest level
;; sequential items. The result should be a sequence of sequences with
;; only one level of nesting.
(defn p93 [__]
  [(= (__ [["Do"] ["Nothing"]])
      [["Do"] ["Nothing"]])
   (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
      [[:a :b] [:c :d] [:e :f]])
   (= (__ '((1 2)((3 4)((((5 6)))))))
      '((1 2)(3 4)(5 6)))])
(deftest t93
  (let [f (fn [vs]
            (let [good? (fn [x]
                          (and (sequential? x)
                               (->> x first sequential? not)))]
              (->> vs
                   (tree-seq sequential? seq)
                   (filter good?))))]
    (is (all? (p93 f)))))

(defn p95 [__]
  [(= (__ '(:a (:b nil nil) nil))
      true)
   (= (__ '(:a (:b nil nil)))
      false)
   (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
      true)
   (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
      false)
   (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
      true)
   (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
      false)
   (= (__ '(:a nil ()))
      false)])
(deftest t95
  (let [f (fn binary-tree? [node]
            (cond
              (nil? node) true
              (not (sequential? node)) false
              (not= 3 (count node)) false
              :else (let [[_ L R] node]
                      (and (binary-tree? L) (binary-tree? R)))))]
    (is (all? (p95 f)))))

(defn p96 [__]
  [(= (__ '(:a (:b nil nil) (:b nil nil))) true)
   (= (__ '(:a (:b nil nil) nil)) false)
   (= (__ '(:a (:b nil nil) (:c nil nil))) false)
   (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
      true)
   (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
      false)
   (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
      false)])
(deftest t96
  (let [f (fn [tree]
            (let [mirror-tree
                  (fn mirror-tree [node]
                    (cond
                      (nil? node)
                      node

                      (or (not (sequential? node))
                          (not= 3 (count node)))
                      (throw (IllegalArgumentException. "grr"))

                      :else
                      (let [[v L R] node]
                        [v
                         (mirror-tree R)
                         (mirror-tree L)])))]
              (= tree (mirror-tree tree))))]
    (is (all? (p96 f)))))

(defn p97 [__]
  [(= (__ 1) [1])
   (= (map __ (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
      [1 4 6 4 1]])
   (= (__ 11)
      [1 10 45 120 210 252 210 120 45 10 1])])
(deftest t97
  (let [f (fn [n]
            (let [next-pascal
                  (fn [vs]
                    (if (empty? vs)
                      [1]
                      (let [xs (conj vs 0), ys (reverse xs)]
                        (into [] (map (fn [x y] (+ x y)) xs ys)))))
                  pascal (iterate next-pascal [])]
              (nth pascal n)))]
    (is (all? (p97 f)))))

;; A function f defined on a domain D induces an equivalence relation
;; on D, as follows: a is equivalent to b with respect to f if and
;; only if (f a) is equal to (f b). Write a function with arguments f
;; and D that computes the equivalence classes of D with respect to
;; f.
(defn p98 [__]
  [(= (__ #(* % %) #{-2 -1 0 1 2})
      #{#{0} #{1 -1} #{2 -2}})
   (= (__ #(rem % 3) #{0 1 2 3 4 5 })
      #{#{0 3} #{1 4} #{2 5}})
   (= (__ identity #{0 1 2 3 4})
      #{#{0} #{1} #{2} #{3} #{4}})
   (= (__ (constantly true) #{0 1 2 3 4})
      #{#{0 1 2 3 4}})])
(deftest t98
  (let [f (fn [g d]
            (->> d
                 (group-by g)
                 vals
                 (map set)
                 (into #{})))]
    (is (all? (p98 f)))))

(defn p99 [__]
  [(= (__ 1 1) [1])
   (= (__ 99 9) [8 9 1])
   (= (__ 999 99) [9 8 9 0 1])])
(deftest t99
  (let [f (fn [& xs]
            (let [n (apply *' xs)]
              (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                   (take-while (fn [vs] (some (complement zero?) vs)))
                   (map second)
                   rest
                   reverse
                   (into []))))]
   (is (all? (p99 f)))))

(defn p100 [__]
  [(== (__ 2 3) 6)
   (== (__ 5 3 7) 105)
   (== (__ 1/3 2/5) 2)
   (== (__ 3/4 1/6) 3/2)
   (== (__ 7 5/7 2 3/5) 210)])
(deftest t100
  (let [f (fn [& nums]
            (let [gcd
                  (fn [& nums]
                    (let [[big lil] (sort > nums)]
                      (loop [a big
                             b lil]
                        (let [q (quot a b)
                              r (rem a b)]
                          (if (zero? r)
                            b
                            (recur b r))))))
                  g (reduce gcd nums)
                  product (apply * nums)]
              (/ product g)))]
    (is (all? (p100 f)))))

;; When working with java, you often need to create an object with
;; fieldsLikeThis, but you'd rather work with a hashmap that has
;; :keys-like-this until it's time to convert. Write a function which
;; takes lower-case hyphen-separated strings and converts them to
;; camel-case strings.
(defn p102 [__]
  [(= (__ "something") "something")
   (= (__ "multi-word-key") "multiWordKey")
   (= (__ "leaveMeAlone") "leaveMeAlone")])
(deftest t102
  (let [f (fn [s]
            (let [join (partial clojure.string/join "")
                  uc clojure.string/upper-case
                  ucfirst (fn [[c & cs :as s']]
                            (if (seq s')
                              (->> cs
                                   (apply str)
                                   (into [(uc c)])
                                   join)
                              s'))
                  [w & ws] (clojure.string/split s #"-+")]
              (->> ws
                   (map ucfirst)
                   (into [w])
                   join)))]
    (is (all? (p102 f)))))

;; Given a sequence S consisting of n elements generate all k-combinations
;; of S, i. e. generate all possible sets consisting of k distinct
;; elements taken from S. The number of k-combinations for a sequence
;; is equal to the binomial coefficient.
(defn p103 [__]
  [(= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
   (= (__ 10 #{4 5 6}) #{})
   (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
   (= (__ 3 #{0 1 2 3 4})
      #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
        #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
   (= (__ 4 #{[1 2 3] :a "abc" "efg"})
      #{#{[1 2 3] :a "abc" "efg"}})
   (= (__ 2 #{[1 2 3] :a "abc" "efg"})
      #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
        #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})])
(deftest t103
  (let [k-bits (fn [k upper]
                 (->> (range upper)
                      (map (fn [n]
                             (let [fq (->> n
                                           Integer/toBinaryString
                                           seq
                                           frequencies)]
                              [n (fq \1)])))
                      (filter (fn [[_ ones]] (= ones k)))
                      (map first)))
        bits->indices (fn [n]
                        (loop [x n, acc [], i 0]
                          (if (zero? x)
                            acc
                            (let [q (quot x 2)
                                  r (rem x 2)
                                  acc' (if (= 1 r) (conj acc i) acc)]
                              (recur q acc' (inc i))))))
        f (fn [k vs]
            (let [upper (apply *' (repeat (count vs) 2))
                  xs (vec vs)]
              (->> (for [b (k-bits k upper)]
                     (let [idxs (bits->indices b)]
                       (->> (map (partial nth xs) idxs)
                            (into #{}))))
                   (into #{}))))]
    (is (all? (p103 f)))))

;; This is the inverse of Problem 92, but much easier. Given an integer
;; smaller than 4000, return the corresponding roman numeral in
;; uppercase, adhering to the subtractive principle
;; (http://www.numericana.com/answer/roman.htm#valid).
(defn p104 [__]
  [(= "I" (__ 1))
   (= "XXX" (__ 30))
   (= "IV" (__ 4))
   (= "CXL" (__ 140))
   (= "DCCCXXVII" (__ 827))
   (= "MMMCMXCIX" (__ 3999))
   (= "XLVIII" (__ 48))])
;; digit   10^3         10^2    10^1    10^0
;;     1   M            C       X       I
;;     2   MM           CC      XX      II
;;     3   MMM          CCC     XXX     III
;;     4   MMMM         CD      XL      IV
;;     5   MMMMM        D       L       V
;;     6   MMMMMM       DC      LX      VI
;;     7   MMMMMMM      DCC     LXX     VII
;;     8   MMMMMMMM     DCCC    LXXX    VIII
;;     9   MMMMMMMMM    CM      XC      IX
(deftest t104
  (let [powers (map #(apply * (repeat % 10)) (reverse (range 4)))
        powahs (partial zipmap powers)
        lookup (->> [["M" "C" "X" "I"]
                     ["MM" "CC" "XX" "II"]
                     ["MMM" "CCC" "XXX" "III"]
                     ["MMMM" "CD" "XL" "IV"]
                     ["MMMMM" "D" "L" "V"]
                     ["MMMMMM" "DC" "LX" "VI"]
                     ["MMMMMMM" "DCC" "LXX" "VII"]
                     ["MMMMMMMM" "DCCC" "LXXX" "VIII"]
                     ["MMMMMMMMM" "CM" "XC" "IX"]]
                    (map (fn [i vs] [i (powahs vs)])
                         (rest (range)))
                    (into {}))
        ->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                        (take-while (fn [vs] (some (complement zero?) vs)))
                        (map second)
                        rest
                        reverse
                        (into [])))
        f (fn [n]
            (let [digits (->digits n)
                  places (->> (count digits)
                              range
                              reverse
                              (map #(apply * (repeat % 10))))]
              (->> (map vector digits places)
                   (remove (fn [[d _]] (zero? d)))
                   (map (fn [ks] (get-in lookup ks)))
                   (apply str))))]
    (is (all? (p104 f)))))

;; Given an input sequence of keywords and numbers, create a map such
;; that each key in the map is a keyword, and the value is a sequence
;; of all the numbers (if any) between it and the next keyword in the
;; sequence.
(defn p105 [__]
  [(= {} (__ []))
   (= {:a [1]} (__ [:a 1]))
   (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
   (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))])
(deftest t105
  (let [f (fn [xs]
            (into {}
                  (loop [acc [], xs xs]
                       (if (seq xs)
                         (let [k (first xs)
                               xs (rest xs)
                               vs (->> xs
                                       (take-while (complement keyword?))
                                       vec)
                               xs' (drop-while (complement keyword?) xs)
                               acc' (conj acc [k vs])]
                           (recur acc' xs'))
                         acc))))]
    (is (all? (p105 f)))))

(defn p107 [__]
  [(= 256 ((__ 2) 16),
      ((__ 8) 2))
   (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
   (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))])
(deftest t107
  (let [f (fn [exponent]
            (fn [base]
              (reduce * (repeat exponent base))))]
    (is (all? (p107 f)))))

;; Given any number of sequences, each sorted from smallest to largest,
;; find the smallest single number which appears in all of the sequences.
;; The sequences may be infinite, so be careful to search lazily.
(defn p108 [__]
  [(= 3 (__ [3 4 5]))
   (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
   (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
   (= 64 (__ (map #(* % % %) (range))                      ;; perfect cubes
             (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
             (iterate inc 20)))                  ;; at least as large as 20
   ])
(deftest t108
  (let [f (fn [& vss]
            (if (== 1 (count vss))
              (ffirst vss)
              (loop [xss vss]
                (if (every? seq xss)
                  (if (apply = (map first xss))
                    (ffirst xss)
                    (let [grp (group-by first xss)
                          ks (sort (keys grp))
                          [k t] ks
                          rss (->> (dissoc grp k)
                                   vals
                                   (apply concat))]
                      (recur
                       (into rss (map (fn [xs]
                                        (drop-while #(< % t) xs))
                                      (grp k))))))
                  nil))))]
    (is (all? (p108 f)))))

;; Write a function that returns a lazy sequence of "pronunciations"
;; of a sequence of numbers. A pronunciation of each element in the
;; sequence consists of the number of repeating identical numbers and
;; the number itself. For example, [1 1] is pronounced as [2 1] ("two
;; ones"), which in turn is pronounced as [1 2 1 1] ("one two, one
;; one").
;;
;; Your function should accept an initial sequence of numbers, and
;; return an infinite lazy sequence of pronunciations, each element
;; being a pronunciation of the previous element.
(defn p110 [__]
  [(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
   (= [3 1 2 4] (first (__ [1 1 1 4 4])))
   (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
   (= 338 (count (nth (__ [3 2]) 15)))])
(deftest t110
  (let [pron (fn [vs]
               (loop [vs vs, acc []]
                 (if (seq vs)
                   (let [v (first vs)
                         cnt (->> vs (take-while #(= v %)) count)]
                     (recur (drop cnt vs) (conj acc cnt v)))
                   acc)))
        f (fn [vs]
            (->> vs
                 (iterate pron)
                 rest))]
    (is (all? (p110 f)))))

;; Create a function which takes an integer and a nested collection
;; of integers as arguments. Analyze the elements of the input collection
;; and return a sequence which maintains the nested structure, and
;; which includes all elements starting from the head whose sum is
;; less than or equal to the input integer.
#_(defn p112 [__]
  [(=  (__ 10 [1 2 [3 [4 5] 6] 7])
       '(1 2 (3 (4))))
   (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
       '(1 2 (3 (4 (5 (6 (7)))))))
   (=  (__ 9 (range))
       '(0 1 2 3))
   (=  (__ 1 [[[[[1]]]]])
       '(((((1))))))
   (=  (__ 0 [1 2 [3 [4 5] 6] 7])
       '())
   (=  (__ 0 [0 0 [0 [0]]])
       '(0 0 (0 (0))))
   (=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
       '(-10 (1 (2 3 (4)))))])
#_(deftest t112
  (let [clone* (fn clone*
                 ([node] (clone* node 0))
                 ([node sum]
                  (if-not (sequential? node)
                    (let [v (if (map? node) (:node node) node)]
                      (prn [:v v, :sum sum, :node node])
                      {:node v, :sum (+' sum v)})
                    (if-not (seq node)
                      ()
                      (map (fn [n] (clone* n sum)) node)))))
        f (fn [m tree]
            (loop [n m, acc ()]
              (if (neg? n)
                acc
                )))]
    (is (all? (p112 f)))))

;; take-while is great for filtering sequences, but it limited: you
;; can only examine a single item of the sequence at a time. What if
;; you need to keep track of some state as you go over the sequence?
;;
;; Write a function which accepts an integer n, a predicate p, and a
;; sequence. It should return a lazy sequence of items in the list up
;; to, but not including, the nth item that satisfies the predicate.
(defn p114 [__]
  [(= [2 3 5 7 11 13]
      (__ 4 #(= 2 (mod % 3))
          [2 3 5 7 11 13 17 19 23]))
   (= ["this" "is" "a" "sentence"]
      (__ 3 #(some #{\i} %)
          ["this" "is" "a" "sentence" "i" "wrote"]))
   (= ["this" "is"]
      (__ 1 #{"a"}
          ["this" "is" "a" "sentence" "i" "wrote"]))])
(deftest t114
  (let [f (fn [n p vs]
            (->> (let [cnt (atom n)]
                   (map (fn [v]
                          (let [orly (p v)]
                            [v orly (if orly (swap! cnt dec) @cnt)]))
                        vs))
                 (filter (fn [[_ _ x]] (pos? x)))
                 (map first)))]
    (is (all? (p114 f)))))

;; A balanced number is one whose component digits have the same sum
;; on the left and right halves of the number. Write a function which
;; accepts an integer n, and returns true iff n is balanced.
(defn p115 [__]
  [(= true (__ 11))
   (= true (__ 121))
   (= false (__ 123))
   (= true (__ 0))
   (= false (__ 88099))
   (= true (__ 89098))
   (= true (__ 89089))
   (= (take 20 (filter __ (range)))
      [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])])
(deftest t115
  (let [->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                        (take-while (fn [vs] (some (complement zero?) vs)))
                        (map second)
                        rest
                        reverse
                        (into [])))
        f (fn [n]
            (let [ds (->digits n)
                  h (quot (count ds) 2)
                  hd (take h ds)
                  tl (->> ds rseq (take h))]
              (= (apply +' hd)
                 (apply +' tl))))]
    (is (all? (p115 f)))))

;; A balanced prime is a prime number which is also the mean of the
;; primes directly before and after it in the sequence of valid primes.
;; Create a function which takes an integer n, and returns true iff
;; it is a balanced prime.
(defn p116 [__]
  [(= false (__ 4))
   (= true (__ 563))
   (= 1103 (nth (filter __ (range)) 15))])
(deftest t116
  (let [prime? (fn [n]
                 (let [m (->> n Math/sqrt long)
                       multiple? (fn [x]
                                   (zero? (mod n x)))]
                   (if (< n 2)
                     false
                     (->> (range 2 (inc m))
                          (not-any? multiple?)))))
        primes (filter prime? (range))
        mean (fn [vs]
               (/ (apply +' vs) (count vs)))
        f (fn [n]
            (let [ps (take-while #(<= % n) primes)]
              (and (= n (last ps))
                   (->> primes
                        (drop (- (count ps) 2))
                        (take 3)
                        mean
                        (= n)))))]
    (is (all? (p116 f)))))

(defn p118 [__]
  [(= [3 4 5 6 7]
      (__ inc [2 3 4 5 6]))
   (= (repeat 10 nil)
      (__ (fn [_] nil) (range 10)))
   (= [1000000 1000001]
      (->> (__ inc (range))
           (drop (dec 1000000))
           (take 2)))])
(deftest t118
  (let [f (fn f [g vs]
            (if (seq vs)
              (lazy-seq (cons (g (first vs))
                              (f g (rest vs))))
              vs))]
    (is (all? (p118 f)))))

;; Write a function which takes a collection of integers as an argument.
;; Return the count of how many elements are smaller than the sum of
;; their squared component digits. For example: 10 is larger than 1
;; squared plus 0 squared; whereas 15 is smaller than 1 squared plus
;; 5 squared.
(defn p120 [__]
  [(= 8 (__ (range 10)))
   (= 19 (__ (range 30)))
   (= 50 (__ (range 100)))
   (= 50 (__ (range 1000)))])
(deftest t120
  (let [f
        (fn [ns]
          (let [->digits
                (fn [n]
                  (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                       (take-while (fn [vs] (some (complement zero?) vs)))
                       (map second)
                       rest
                       reverse
                       (into [])))

                sumsq-of-digits
                (fn [n]
                  (->> n
                       ->digits
                       (map (fn [x] (*' x x)))
                       (apply +')))]

              (->> ns
                   (map (fn [n] [n (sumsq-of-digits n)]))
                   (filter (fn [[n sumsq]] (< n sumsq)))
                   count)))]
    (is (all? (p120 f)))))

;; Given a mathematical formula in prefix notation, return a function
;; that calculates the value of the formula. The formula can contain
;; nested calculations using the four basic mathematical operators,
;; numeric constants, and symbols representing variables. The returned
;; function has to accept a single parameter containing the map of
;; variable names to their values.
(defn p121 [__]
  [(= 2 ((__ '(/ a b))
         '{b 8 a 16}))
   (= 8 ((__ '(+ a b 2))
         '{a 2 b 4}))
   (= [6 0 -4]
      (map (__ '(* (+ 2 a)
                   (- 10 b)))
           '[{a 1 b 8}
             {b 5 a -2}
             {a 2 b 11}]))
   (= 1 ((__ '(/ (+ x 2)
                 (* 3 (+ y 1))))
         '{x 4 y 1}))])
(deftest t121
  (let [ops-tab {'+ +, '- -, '* *, '/ /}
        f (fn [form]
            (fn [sym-tab]
              (let [form' (clojure.walk/postwalk-replace
                           (merge ops-tab sym-tab)
                           form)
                    wark (fn wark [expr]
                           (if-not (list? expr)
                             expr
                             (apply (first expr)
                                    (map wark (rest expr)))))]
                (wark form'))))]
    (is (all? (p121 f)))))

(defn p122 [__]
  [(= 0     (__ "0"))
   (= 7     (__ "111"))
   (= 8     (__ "1000"))
   (= 9     (__ "1001"))
   (= 255   (__ "11111111"))
   (= 1365  (__ "10101010101"))
   (= 65535 (__ "1111111111111111"))])
(deftest t122
  (let [g (fn [x]
            (->> (/ (Math/log (inc x)) (Math/log 2))
                 Math/ceil
                 long
                 (max 1)
                 range
                 (map (fn [i] (bit-test x i)))
                 (map {true \1, false \0})
                 reverse
                 clojure.string/join))
        f (fn [s]
            (->> s
                 (map {\1 1, \0 0})
                 (reduce (fn [acc b]
                           (+' (*' 2 acc) b))
                         0)))]
    (is (all? (p122 f)))))

(defn p126 [__]
  (let [x __]
    (and (= (class x) x)
         x)))
(deftest t126
  (is (p126 java.lang.Class)))

;; A standard American deck of playing cards has four suits - spades,
;; hearts, diamonds, and clubs - and thirteen cards in each suit. Two
;; is the lowest rank, followed by other integers up to ten; then the
;; jack, queen, king, and ace.
;;
;; It's convenient for humans to represent these cards as suit/rank
;; pairs, such as H5 or DQ: the heart five and diamond queen respectively.
;; But these forms are not convenient for programmers, so to write a
;; card game you need some way to parse an input string into meaningful
;; components. For purposes of determining rank, we will define the
;; cards to be valued from 0 (the two) to 12 (the ace)
;;
;; Write a function which converts (for example) the string "SJ" into
;; a map of {:suit :spade, :rank 9}. A ten will always be represented
;; with the single character "T", rather than the two characters "10".
(defn p128 [__]
  [(= {:suit :diamond :rank 10} (__ "DQ"))
   (= {:suit :heart :rank 3} (__ "H5"))
   (= {:suit :club :rank 12} (__ "CA"))
   (= (range 13) (map (comp :rank __ str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))])
(deftest t128
  (let [f (fn [[s r]]
            (let [char->suit {\D :diamond
                              \H :heart
                              \C :club
                              \S :spade}
                  char->rank (->> "23456789TJQKA"
                                  (map-indexed (comp vec reverse vector))
                                  (into {}))]
              {:suit (char->suit s)
               :rank (char->rank r)}))]
    (is (all? (p128 f)))))

;; Given a variable number of sets of integers, create a function which
;; returns true iff all of the sets have a non-empty subset with an
;; equivalent summation.
(defn p131 [__]
  [(= true  (__ #{-1 1 99}
                #{-2 2 888}
                #{-3 3 7777})) ; ex. all sets have a subset which sums to zero
   (= false (__ #{1}
                #{2}
                #{3}
                #{4}))
   (= true  (__ #{1}))
   (= false (__ #{1 -3 51 9}
                #{0}
                #{9 2 81 33}))
   (= true  (__ #{1 3 5}
                #{9 11 4}
                #{-3 12 3}
                #{-3 4 -2 10}))
   (= false (__ #{-1 -2 -3 -4 -5 -6}
                #{1 2 3 4 5 6 7 8 9}))
   (= true  (__ #{1 3 5 7}
                #{2 4 6 8}))
   (= true  (__ #{-1 3 -5 7 -9 11 -13 15}
                #{1 -3 5 -7 9 -11 13 -15}
                #{1 -1 2 -2 4 -4 8 -8}))
   (= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
                #{10 -9 8 -7 6 -5 4 -3 2 -1}))])
(deftest t131
 (let [bits->indices (fn [n]
                       (loop [x n, acc [], i 0]
                         (if (zero? x)
                           acc
                           (let [q (quot x 2)
                                 r (rem x 2)
                                 acc' (if (= 1 r) (conj acc i) acc)]
                             (recur q acc' (inc i))))))
       power-set (fn [u]
                   (let [n (apply *' (repeat (count u) 2))
                         v (vec u)]
                     (->> (for [b (range n)]
                            (let [idxs (bits->indices b)]
                              (->> (map (partial nth v) idxs)
                                   (into #{}))))
                          (into #{}))))
       f (fn [& the-sets]
           (->> (for [a-set the-sets]
                  (->> a-set
                       power-set
                       (remove empty?)
                       (map (partial reduce +' 0))
                       (into #{})))
                (apply clojure.set/intersection)
                ((complement empty?))))]
   (is (all? (p131 f)))))

;; Write a function that takes a two-argument predicate, a value, and
;; a collection; and returns a new collection where the value is
;; inserted between every two items that satisfy the predicate.
(defn p132 [__]
  [(= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
   (= '(2) (__ > :more [2]))
   (= [0 1 :x 2 :x 3 :x 4] (__ #(and (pos? %) (< % %2)) :x (range 5)))
   (empty? (__ > :more ()))
   (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
      (take 12 (->> [0 1]
                    (iterate (fn [[a b]] [b (+ a b)]))
                    (map first) ; fibonacci numbers
                    (__ (fn [a b] ; both even or both odd
                          (= (mod a 2) (mod b 2)))
                        :same))))])
(deftest t132
  (let [f (fn [p x vs]
            (if-not (seq vs)
              vs
              (->> vs
                   (partition-all 2 1)
                   (mapcat (fn [[a b :as entry]]
                             (if (= 1 (count entry))
                               [a]
                               (if (p a b)
                                 [a x]
                                 [a])))))))
        #_(fn [p x vs]
            (if (seq vs)
             (conj
              (reduce (fn [acc [a b]]
                        (if (p a b)
                          (conj acc a x)
                          (conj acc a)))
                      []
                      (map vector vs (rest vs)))
              (last vs))
             vs))]
    (is (all? (p132 f)))))

;; Write a function which, given a key and map, returns true
;; iff the map contains an entry with that key and its value is nil.
(defn p134 [__]
  [(true?  (__ :a {:a nil :b 2}))
   (false? (__ :b {:a nil :b 2}))
   (false? (__ :c {:a nil :b 2}))])
(deftest t134
  (let [f (fn [k m]
            (boolean (and (find m k)
                          (nil? (get m k)))))]
    (is (all? (p134 f)))))

(defn p135 [__]
  [(= 7  (__ 2 + 5))
   (= 42 (__ 38 + 48 - 2 / 2))
   (= 8  (__ 10 / 2 - 1 * 2))
   (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))])
(deftest t135
  (let [f (fn [& vs]
            {:pre [(odd? (count vs))
                   (every? fn?
                           (->> (map-indexed vector vs)
                                (filter (fn [[i _]] (odd? i)))
                                (map second)))
                   (every? number?
                           (->> (map-indexed vector vs)
                                (filter (fn [[i _]] (even? i)))
                                (map second)))]}
            (reduce (fn [acc [op x]]
                      (op acc x))
                    (first vs)
                    (->> (rest vs) (partition 2))))]
    (is (all? (p135 f)))))

;; Write a function which returns a sequence of digits of a non-negative
;; number (first argument) in numerical system with an arbitrary base
;; (second argument). Digits should be represented with their integer
;; values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and
;; [15] in base 16.
(defn p137 [__]
  [(= [1 2 3 4 5 0 1] (__ 1234501 10))
   (= [0] (__ 0 11))
   (= [1 0 0 1] (__ 9 2))
   (= [1 0] (let [n (rand-int 100000)](__ n n)))
   (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))])
(deftest t137
  (let [f (fn [n b]
            {:pre [((complement neg?) n)
                   (<= 2 b)]}
            (if (zero? n)
              [0]
              (rseq
               (loop [x n, acc []]
                 (if (zero? x)
                   acc
                   (let [q (quot x b)
                         r (rem x b)
                         acc' (conj acc r)]
                     (recur q acc')))))))]
    (is (all? (p137 f)))))

;; In trick-taking card games such as bridge, spades, or hearts, cards
;; are played in groups known as "tricks" - each player plays a single
;; card, in order; the first player is said to "lead" to the trick.
;; After all players have played, one card is said to have "won" the
;; trick. How the winner is determined will vary by game, but generally
;; the winner is the highest card played in the suit that was led.
;; Sometimes (again varying by game), a particular suit will be
;; designated "trump", meaning that its cards are more powerful than
;; any others: if there is a trump suit, and any trumps are played,
;; then the highest trump wins regardless of what was led.
;;
;; Your goal is to devise a function that can determine which of a
;; number of cards has won a trick. You should accept a trump suit,
;; and return a function winner. Winner will be called on a sequence
;; of cards, and should return the one which wins the trick. Cards
;; will be represented in the format returned by Problem 128, Recognize
;; Playing Cards: a hash-map of :suit and a numeric :rank. Cards with
;; a larger rank are stronger.
(defn p141 [__]
  [(let [notrump (__ nil)]
     (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                              {:suit :club :rank 9}]))
          (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                              {:suit :club :rank 10}]))))
   (= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                          {:suit :club :rank 10}]))
   (= {:suit :heart :rank 8}
      ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                    {:suit :diamond :rank 10} {:suit :heart :rank 4}]))])
(deftest t141
  (let [winner-for-suit (fn [suit cards]
                          (->> cards
                               (filter (fn [c] (= suit (:suit c))))
                               (sort-by (fn [c] (:rank c)) >)
                               first))
        f (fn [trump-suit]
            (fn [cards]
              (if-not trump-suit
                (winner-for-suit (->> cards first :suit) cards)
                (if-let [trumper (winner-for-suit trump-suit cards)]
                  trumper
                  (winner-for-suit (->> cards first :suit) cards)))))]
    (is (all? (p141 f)))))


(defn p143 [__]
  [(= 0 (__ [0 1 0] [1 0 0]))
   (= 3 (__ [1 1 1] [1 1 1]))
   (= 32 (__ [1 2 3] [4 5 6]))
   (= 256 (__ [2 5 6] [100 10 1]))])
(deftest t143
  (let [f (fn [xs ys]
            (->> (map *' xs ys)
                 (apply +')))]
    (is (all? (p143 f)))))

;; Write an oscillating iterate: a function that takes an initial value
;; and a variable number of functions. It should return a lazy sequence
;; of the functions applied to the value in order, restarting from the
;; first function after it hits the end.
(defn p144 [__]
  [(= (take 3 (__ 3.14 int double))
      [3.14 3 3.0])
   (= (take 5 (__ 3 #(- % 3) #(+ 5 %)))
      [3 0 5 2 7])
   (= (take 12 (__ 0 inc dec inc dec inc))
      [0 1 0 1 0 1 2 1 2 1 2 3])])
(deftest t144
  (let [f (fn [v & gs]
            (let [foo (fn [[x fns]]
                        (let [a-fn (first fns)]
                          [(a-fn x) (rest fns)]))]
              (->> (iterate foo [v (cycle gs)])
                   (map first))))]
    (is (all? (p144 f)))))

(defn p145 [__]
  [(= __ (for [x (range 40)
            :when (= 1 (rem x 4))]
           x))
   (= __ (for [x (iterate #(+ 4 %) 0)
            :let [z (inc x)]
            :while (< z 40)]
           z))
   (= __ (for [[x y] (partition 2 (range 20))]
           (+ x y)))])
(deftest t145
  (is (all? (p145 (map (comp inc (partial * 4)) (range 10))))))

;; Because Clojure's for macro allows you to "walk" over multiple
;; sequences in a nested fashion, it is excellent for transforming all
;; sorts of sequences. If you don't want a sequence as your final
;; output (say you want a map), you are often still best-off using
;; for, because you can produce a sequence and feed it into a map, for
;; example.
;;
;; For this problem, your goal is to "flatten" a map of hashmaps. Each
;; key in your output map should be the "path"1 that you would have
;; to take in the original map to get to a value, so for example {1
;; {2 3}} should result in {[1 2] 3}. You only need to flatten one
;; level of maps: if one of the values is a map, just leave it alone.
;;
;; 1 That is, (get-in original [k1 k2]) should be the same as (get
;; result [k1 k2])
(defn p146 [__]
  [(= (__ '{a {p 1, q 2}
            b {m 3, n 4}})
      '{[a p] 1, [a q] 2
        [b m] 3, [b n] 4})
   (= (__ '{[1] {a b c d}
            [2] {q r s t u v w x}})
      '{[[1] a] b, [[1] c] d,
        [[2] q] r, [[2] s] t,
        [[2] u] v, [[2] w] x})
   (= (__ '{m {1 [a b c] 3 nil}})
      '{[m 1] [a b c], [m 3] nil})])
(deftest t146
  (let [f (fn [m]
            (into {}
                  (for [[k v] m
                        [k' v'] v]
                    [[k k'] v'])))]
    (is (all? (p146 f)))))

;; Write a function that, for any given input vector of numbers, returns
;; an infinite lazy sequence of vectors, where each next one is
;; constructed from the previous following the rules used in Pascal's
;; Triangle. For example, for [3 1 2], the next row is [3 4 3 2].
;;
;; Beware of arithmetic overflow! In clojure (since version 1.3 in
;; 2011), if you use an arithmetic operator like + and the result is
;; too large to fit into a 64-bit integer, an exception is thrown. You
;; can use +' to indicate that you would rather overflow into Clojure's
;; slower, arbitrary-precision bigint.
(defn p147 [__]
  [(= (second (__ [2 3 2])) [2 5 5 2])
   (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
   (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
   (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))])
(deftest t147
  (let [f (fn [vs]
            {:pre [(seq vs)]}
            (let [successor (fn [ns]
                              (->> ns
                                   (partition-all 2 1)
                                   (into [(list (first ns))])
                                   (map (partial apply +'))))]
              (iterate successor vs)))]
    (is (all? (p147 f)))))

;; Write a function which calculates the sum of all natural numbers
;; under n (first argument) which are evenly divisible by at least one
;; of a and b (second and third argument). Numbers a and b are guaranteed
;; to be coprimes.
;;
;; Note: Some test cases have a very large n, so the most obvious
;; solution will exceed the time limit.
(defn p148 [__]
  [(= 0 (__ 3 17 11))
   (= 23 (__ 10 3 5))
   (= 233168 (__ 1000 3 5))
   (= "2333333316666668" (str (__ 100000000 3 5)))
   (= "110389610389889610389610"
      (str (__ (* 10000 10000 10000) 7 11)))
   (= "1277732511922987429116"
      (str (__ (* 10000 10000 10000) 757 809)))
   (= "4530161696788274281"
      (str (__ (* 10000 10000 1000) 1597 3571)))])
(deftest t148
  (let [n-sum (fn [n] (/ (*' n (inc' n)) 2))
        m-sum (fn [n m]
                (*' m (n-sum (quot (dec n) m))))
        f (fn [n a b]
            (-' (+' (m-sum n a) (m-sum n b))
                (m-sum n (*' a b))))]
    (is (all? (p148 f)))))

;; A palindromic number is a number that is the same when written
;; forwards or backwards (e.g., 3, 99, 14341).
;;
;; Write a function which takes an integer n, as its only argument,
;; and returns an increasing lazy sequence of all palindromic numbers
;; that are not less than n.
;;
;; The most simple solution will exceed the time limit!
(defn p150 [__]
  [(= (take 26 (__ 0))
      [0 1 2 3 4 5 6 7 8 9
       11 22 33 44 55 66 77 88 99
       101 111 121 131 141 151 161])
   (= (take 16 (__ 162))
      [171 181 191 202
       212 222 232 242
       252 262 272 282
       292 303 313 323])
   (= (take 6 (__ 1234550000))
      [1234554321 1234664321 1234774321
       1234884321 1234994321 1235005321])
   (= (first (__ (* 111111111 111111111)))
      (* 111111111 111111111))
   (= (set (take 199 (__ 0)))
      (set (map #(first (__ %)) (range 0 10000))))
   (= true
      (apply < (take 6666 (__ 9999999))))
   (= (nth (__ 0) 10101)
      9102019)])
(deftest t150
  (let [pow10 (fn [n] (apply *' (repeat n 10)))
        num-digits (fn [n] (->> n inc Math/log10 Math/ceil int))
        ->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                        (take-while (fn [vs] (some (complement zero?) vs)))
                        (map second)
                        rest
                        reverse
                        (into [])))
        digit-at (fn [n i]
                   (-> n
                       (quot (pow10 i))
                       (rem 10)))
        left-digits (fn [n]
                      (let [cnt (num-digits n)
                            h (quot cnt 2)]
                        (quot n (pow10 h))))
        flip-digits (fn [n]
                      (->> n
                           ->digits
                           (map-indexed vector)
                           (reduce (fn [acc [p d]]
                                     (+' acc (*' d (pow10 p))))
                                   0)))
        mirror-digits (fn [lhs & [skip-middle?]]
                        (let [cnt (num-digits lhs)
                              m (if skip-middle? (dec cnt) cnt)
                              base (*' lhs (pow10 m))
                              v (if skip-middle?
                                  (quot lhs 10)
                                  lhs)]
                          (+' base (flip-digits v))))
        pal? (fn [n]
               (let [cnt (num-digits n)]
                 (every? (fn [i]
                           (= (digit-at n i)
                              (digit-at n (- cnt i 1))))
                         (range (quot cnt 2)))))
        all-9? (fn [x]
                 (->> x
                      ->digits
                      (every? #(= 9 %))))
        pal-inc (fn [x]
                  (if (or (< x 10) (not (pal? x)))
                    (inc x)
                    (let [cnt (num-digits x)
                          lhs (left-digits x)
                          nines? (all-9? x)
                          lhs (if (and nines? (odd? cnt))
                                (quot lhs 10)
                                lhs)]
                      (mirror-digits (inc' lhs)
                                     (if nines?
                                       (even? cnt)
                                       (odd? cnt))))))
        f (fn [n]
            (->> (iterate pal-inc n)
                 (filter pal?)))]
    (is (all? (p150 f)))))

;; Given a set of sets, create a function which returns true if no two
;; of those sets have any elements in common1 and false otherwise.
;; Some of the test cases are a bit tricky, so pay a little more
;; attention to them.
;;
;; 1 Such sets are usually called pairwise disjoint or mutually disjoint.
(defn p153 [__]
  [(= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})    ;; 1
      true)
   (= (__ #{#{:a :b :c :d :e}                           ;; 2
            #{:a :b :c :d}
            #{:a :b :c}
            #{:a :b}
            #{:a}})
      false)
   (= (__ #{#{[1 2 3] [4 5]}                            ;; 3
            #{[1 2] [3 4 5]}
            #{[1] [2] 3 4 5}
            #{1 2 [3 4] [5]}})
      true)
   (= (__ #{#{'a 'b}                                    ;; 4
            #{'c 'd 'e}
            #{'f 'g 'h 'i}
            #{''a ''c ''f}})
      true)
   (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
            #{#{:x :y :z} #{:x :y} #{:z} #{}}
            #{'[:x :y :z] [:x :y] [:z] [] {}}})
      false)
   (= (__ #{#{(= "true") false}
            #{:yes :no}
            #{(class 1) 0}
            #{(symbol "true") 'false}
            #{(keyword "yes") ::no}
            #{(class '1) (int \0)}})
      false)
   (= (__ #{#{distinct?}
            #{#(-> %) #(-> %)}
            #{#(-> %) #(-> %) #(-> %)}
            #{#(-> %) #(-> %) #(-> %)}})
      true)
   (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
            #{'+ '* mapcat (comment mapcat)}
            #{(do) set contains? nil?}
            #{, , , #_, , empty?}})
      false)])
(deftest t153
  (let [f (fn [vs]
            (->> vs
                 (map (fn [a-set]
                        (->> a-set
                             (map (fn [v]
                                    (if (or (seq? v) (set? v))
                                      (vec v)
                                      v)))
                             (into #{}))))
                 (into #{})
                 (reduce (partial clojure.set/intersection))
                 empty?))
        g (fn [the-sets]
            (let [other-sets (fn [a-set]
                               (->> (disj the-sets a-set)
                                    (apply clojure.set/union)))]
              (->> the-sets
                   (map (fn [a-set]
                          [a-set (other-sets a-set)]))
                   (every? (fn [[a-set remainder]]
                             (->> (clojure.set/intersection a-set remainder)
                                  empty?))))))]
    (is (all? (p153 g)))))

;; When retrieving values from a map, you can specify default values
;; in case the key is not found:
;;   (= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default
;; values? Write a function which takes a default value and a sequence
;; of keys and constructs a map.
(defn p156 [__]
  [(= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
   (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
   (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})])
(deftest t156
  (let [f (fn [v ks]
            (->> (for [k ks]
                   [k v])
                 (into {})))]
    (is (all? (p156 f)))))

(defn p157 [__]
  [(= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
   (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
   (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])])
(deftest t157
  (let [f (fn [vs]
            (->> vs
                 (map-indexed vector)
                 (map reverse)))]
    (is (all? (p157 f)))))

;; Write a function that accepts a curried function of unknown arity
;; n. Return an equivalent function of n arguments.  You may wish to
;; read https://en.wikipedia.org/wiki/Currying.
(defn p158 [__]
  [(= 10 ((__ (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (+ a b c d))))))
          1 2 3 4))
   (= 24 ((__ (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (* a b c d))))))
          1 2 3 4))
   (= 25 ((__ (fn [a]
                (fn [b]
                  (* a b))))
          5 5))])
(deftest t158
  (let [f (fn [g]
            (fn [& args]
              (reduce (fn [acc v]
                        (acc v))
                      g
                      args)))]
    (is (all? (p158 f)))))

;; Set A is a subset of set B, or equivalently B is a superset of A,
;; if A is "contained" inside B. A and B may coincide.
(defn p161 [__]
  [(clojure.set/superset? __ #{2})
   (clojure.set/subset? #{1} __)
   (clojure.set/superset? __ #{1 2})
   (clojure.set/subset? #{1 2} __)])
(deftest t161
  (is (all? (p161 #{1 2}))))

;; In Clojure, only nil and false represent the values of logical
;; falsity in conditional tests - anything else is logical truth.
(defn p162 [__]
  [(= __ (if-not false 1 0))
   (= __ (if-not nil 1 0))
   (= __ (if true 1 0))
   (= __ (if [] 1 0))
   (= __ (if [0] 1 0))
   (= __ (if 0 1 0))
   (= __ (if 1 1 0))])
(deftest t162
  (is (all? (p162 1))))

(defn p166 [__]
  [(= :gt (__ < 5 1))
   (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
   (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
   (= :gt (__ > 0 2))])
(deftest t166
  (let [f (fn [cmp v1 v2]
            (cond
              (cmp v1 v2) :lt
              (cmp v2 v1) :gt
              :else :eq))]
    (is (all? (p166 f)))))

;; Write a function that takes a sequence of integers and returns a
;; sequence of "intervals". Each interval is a a vector of two integers,
;; start and end, such that all integers between start and end (inclusive)
;; are contained in the input sequence.
(defn p171 [__]
  [(= (__ [1 2 3]) [[1 3]])
   (= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
   (= (__ [1 1 1 1 1 1 1]) [[1 1]])
   (= (__ []) [])
   (= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
      [[1 4] [6 6] [9 11] [13 17] [19 19]])])
(deftest t171
  (let [f (fn [vs]
            (->> vs
                 (into #{})
                 sort
                 (reduce (fn [acc v]
                           (if-not (seq acc)
                             (conj acc [v])
                             (let [u (peek (peek acc))]
                               (if (= 1 (- v u))
                                 (update-in acc
                                            [(dec (count acc))]
                                            conj v)
                                 (conj acc [v])))))
                         [])
                 (map (juxt first last))))]
    (is (all? (p171 f)))))

(defn p173 []
  (= 3
     (let [[op v] [+ (range 3)]] (apply op v))
     (let [[[op v] b] [[+ 1] 2]] (op v b))
     (let [[op v] [inc 2]] (op v))))
(deftest t173
  (is (p173)))

;; When parsing a snippet of code it's often a good idea to do a sanity
;; check to see if all the brackets match up. Write a function that
;; takes in a string and returns truthy if all square [ ] round ( )
;; and curly { } brackets are properly paired and legally nested, or
;; returns falsey otherwise.
(defn p177 [__]
  [(__ "This string has no brackets.")
   (__ "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")
   (not (__ "(start, end]"))
   (not (__ "())"))
   (not (__ "[ { ] } "))
   (__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
   (not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))
   (not (__ "["))])
(deftest t177
  (let [f (fn [s]
            (let [o->c (into {} (map vector "([{" ")]}"))]
              (->> s
                   seq
                   (filter (->> "()[]{}" seq set))
                   (reduce (fn [acc c]
                             (if-not (seq acc)
                               (conj acc c)
                               (if ((->> "([{" seq set) c)
                                 (conj acc c)
                                 (if (= c (o->c (peek acc)))
                                   (pop acc)
                                   #_(reduced [:flips-table])
                                   (conj acc c)))))
                           [])
                   empty?)))]
    (is (all? (p177 f)))))
