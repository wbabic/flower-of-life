(ns edn-play.power-set)

(comment
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
  )
