(ns edn-play.power-set)

(comment
  "use reductions to see the process"
  (defn power-set [s]
  (reductions (fn [ss x]
                (concat ss (map #(conj % x) ss)))
              '([])
              s)))

(defn power-set [s]
  (reduce (fn [ss x]
            (concat ss (map #(conj % x) ss)))
          '([])
          s))


(comment
  (defn m [ss x]
    (map #(conj % x) ss))

  (defn c [ss x]
    (concat ss
            (map #(conj % x) ss)))

  (def ss '([]))
  (m ss 1)
  ;;=> ([1])
  (concat (m ss 1) ss)
  ;;=> ([1] [])
  (c ss 1)
  ;;=> ([] [1])
  (def ss2 '([1] []))
  (concat (m ss2 2) ss2)
  ;;=> ([1 2] [2] [1] [])
  (c ss2 2)
  ;;=> ([1] [] [1 2] [2])
  )

(comment
  (map (partial reduce +) (power-set #{1 2 3}))
  )

(defn sum-set [s]
  (map (partial reduce +)
       (power-set s)))

(comment
  (power-set #{1 2 3})
  ;;=> ([] [1] [2] [1 2] [3] [1 3] [2 3] [1 2 3])
  (sum-set #{1 2 3})
  ;;=> (0 1 2 3 3 4 5 6)
  )

(defn sum-nonempty-set [s]
  (map (partial reduce +)
       (filter (complement empty?) (power-set s))))

(comment
  (sum-nonempty-set #{1 2 3})
  ;;=> (1 2 3 3 4 5 6)
  )

(defn some-subset-equal-sum [sets]
  (not (empty? (apply clojure.set/intersection
                      (map (comp set sum-nonempty-set) sets)))))

(comment
  (def sample-set-1
    (list
     #{-1 3 -5 7 -9 11 -13 15}
     #{1 -3 5 -7 9 -11 13 -15}
     #{1 -1 2 -2 4 -4 8 -8}))
  (def sample-set-2
    (list
     #{-1 -2 -3 -4 -5 -6}
     #{1 2 3 4 5 6 7 8 9}))
  (def sample-set-3
    (list
     #{1 2} #{10 20}))
  
  ((comp set sum-nonempty-set) (first sample-set-1))
  
  (some-subset-equal-sum sample-set-1)
  (some-subset-equal-sum sample-set-2)

  )

(defn power-set-2 [sets]
  (map
   (fn [set] (filter (complement empty?)
                    (reduce
                     (fn [ss x]
                       (concat ss
                               (map
                                (fn [s] (conj s x))
                                ss)))
                     [[]]
                     set)))
   sets))

(comment
  (power-set-2 sample-set-3)
  ;;=> (([1] [2] [1 2]) ([10] [20] [10 20]))

  (def f '([1] [2] [1 2]))
  ;; sum each vector; change into #{1 2 3}

  (into #{} (map (partial reduce +) f))
  ;;=> #{1 2 3}

  (map
   (fn [p]
     (into #{} (map (partial reduce +) p)))
   (power-set-2 sample-set-3))
  ;;=> (#{1 2 3} #{10 20 30})

  (apply clojure.set/intersection
         (map
          (fn [p]
            (into #{} (map (partial reduce +) p)))
          (power-set-2 sample-set-3)))
  ;;=> #{}

  (apply clojure.set/intersection
         (map
          (fn [p]
            (into #{} (map (partial reduce +) p)))
          (power-set-2 sample-set-1)))
  ;;=> #{-1 0 1 -2 -3 2 3 -4 -5 4 -6 5 -7 6 7 -8 -9 8 -10 9 -11 10 -12 11 -13 12 -14 13 -15 14 15}

  ((complement empty?)
   (apply clojure.set/intersection
          (map
           (fn [p]
             (into #{} (map (partial reduce +) p)))
           (power-set-2 sample-set-1))))
  ;;=> true

  ((complement empty?)
   (apply clojure.set/intersection
          (map
           (fn [p]
             (into #{} (map (partial reduce +) p)))
           (power-set-2 sample-set-3))))
  
  ;;=> false
  (defn s1 [ps]
    ((complement empty?)
     (apply clojure.set/intersection
            (map
             (fn [p]
               (into #{} (map (partial reduce +) p)))
             ps))))

  (s1 (power-set-2 sample-set-3))
  (s1 (power-set-2 sample-set-1))

  ((fn [& sets]
     (let [power-set
           (fn [sets]
             (map
              (fn [set] (filter (complement empty?)
                               (reduce
                                (fn [ss x]
                                  (concat ss
                                          (map
                                           (fn [s] (conj s x))
                                           ss)))
                                [[]]
                                set)))
              sets))]
       ((complement empty?)
        (apply clojure.set/intersection
               (map
                (fn [p]
                  (into #{} (map (partial reduce +) p)))
                (power-set sets))))))
   #{-1 1 99} 
   #{-2 2 888}
   #{-3 3 7777})

  (fn [& sets]
     (let [power-set
           (fn [sets]
             (map
              (fn [set] (filter (complement empty?)
                               (reduce
                                (fn [ss x]
                                  (concat ss
                                          (map
                                           (fn [s] (conj s x))
                                           ss)))
                                [[]]
                                set)))
              sets))]
       ((complement empty?)
        (apply clojure.set/intersection
               (map
                (fn [p]
                  (into #{} (map (partial reduce +) p)))
                (power-set sets))))))
  )
