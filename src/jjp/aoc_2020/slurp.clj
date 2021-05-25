(ns jjp.aoc-2020.slurp)

(->> [1 2 3 4 5]
    (map (partial * 2))
    (reduce *))

(reduce conj
        #{}
        [1 2 3 4 5])

(reduce +
        0
        [1 2 3 4 5])

(reduce (fn [a b]
          (if (> a b)
            a
            b))
        0
        [1 2 3 4 5])

(reduce (fn [[num den] b]
          [(+ num b)
           (inc den)])
        [0 0]
        [1 2 3 4 5])

(def negative-quotient (comp - /))

(comment
  (+ 1 (* 2 3) 4)

  (map (comp record first)
       (d/q '[:find ?post
              :in $ ?search
              :where
              [(fulltext $ :post/content ?search)
               [[?post ?content]]]]
            (db/db)
            (:p params))

       )
  )
