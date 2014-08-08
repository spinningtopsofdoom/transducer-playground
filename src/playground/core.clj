(ns playground.core)
;;First attempt to look into how transducers work
(let [tr (comp
            (filter (fn [x] (do (println (str "Filtering: " x)) (even? x))))
            (map (fn[x] (do (println (str "Mapping: " x)) (inc x))))
            (partition-by (fn [x] (do (println (str "Paritioning: " x)) (< x 5))))
            )]
  (into [] tr (range 10)))
;;Source code reading and renaming of vars for better understinding
(comment
 ;;Map transducer
  ([mapping-function]
    (fn [outer-reducer]
      (fn
        ([] (outer-reducer))
        ([memo-result] (outer-reducer memo-result))
        ([memo-result input]
          (outer-reducer memo-result (mapping-function input)))
        ([result input & inputs]
          (outer-reducer memo-result (apply mapping-function input inputs))))))
;;Filter transducer
  ([filter-predicate]
    (fn [outer-reducer]
      (fn
        ([] (outer-reducer))
        ([memo-result] (outer-reducer result))
        ([memo-result input]
          (if (filter-predicate input)
            (outer-reducer memo-result input)
            memo-result)))))
  ;;Partiton By transducer
  ([partition-function]
     (fn [outer-reducer]
       (let [partition-array (array-list)
             parition-sentinel-container (atom ::none)]
         (fn
           ([] (outer-reducer))
           ([memo-result]
              (let [memo-result (if (.isEmpty partition-array)
                             result
                             (let [partition-vector (vec (.toArray partition-array))]
                               ;;flushing ops must clear before invoking possibly
                               ;;failing nested op, else infinite loop
                               (.clear partition-array)
                               (outer-reducer memo-result partition-vector)))]
                (outer-reducer memo-result partition-vector)))
           ([memo-result input]
              (let [sentinel-value @parition-sentinel-container
                    parition-value (partition-function input)]
                (reset! parition-sentinel-container parition-value)
                (if (or (keyword-identical? sentinel-value ::none)
                        (= partition-value sentinel-value))
                  (do
                    (.add partition-array input)
                    result)
                  (let [partition-vector (vec (.toArray partition-array))]
                    (.clear partition-array)
                    (.add partition-array input)
                    (outer-reducer result partition-vector))))))))))

;;The identity transducer (like a platyapus it really doesn't do much of anything)
(defn identity-transducer []
  (fn [outer-transducer]
    (fn
      ([] (outer-transducer))
      ([memo-result] (outer-transducer memo-result))
      ([memo-result input]
         (outer-transducer memo-result input)))))
;;Lets see if it works.  It does!
(let [transducer (comp
                   (filter even?)
                   (map inc)
                   (identity-transducer)
                   (partition-by #(< % 5)))]
  (into [] transducer (range 10)))
;;Returns [[1 3] [5 7 9]]

;;Lets make a logging transducer to see what is going on inside
(defn logging-transducer [logger]
  (fn [outer-transducer]
    (fn
      ([] (outer-transducer))
      ([memo-result] (outer-transducer memo-result))
      ([memo-result input]
         (do
           (logger memo-result input)
           (outer-transducer memo-result input))))))
;;What do we see when we log what is going on?
;;Log: [[TransientVector 1] [TransientVector 3] [TransientVector 5] [TransientVector 7] [TransientVector 9]]
(let [transducer (comp
                   (filter even?)
                   (map inc)
                   (logging-transducer println)
                   (partition-by #(< % 5)))]
  (into [] transducer (range 10)))
;;Returns [[1 3] [5 7 9]]

;;How about this?
;;Log: [[0 1] [1 3] [4 5] [9 7] [16 9]]
(let [transducer (comp
                   (filter even?)
                   (map inc)
                   (logging-transducer println)
                   )]
  (transduce transducer + 0 (range 10)))
;;Returns 25

;;Now lets change it to use sequence then into. Huh weird we get
;;Log: [[((((() 9] [) 7] [) 5] [) 3] [[9 7 5]) 1]]
;; and the results is [[9 7 5]]!
;;The log looks like the itermediate results are wrapped in something
;; (((((() 9) 7) 5) 3) [9 7 5]) 1 is what is looks like all joined together
(let [transducer (comp
                   (filter even?)
                   (map inc)
                   (logging-transducer println)
                   (partition-by #(< % 5)))]
  (into [] (sequence transducer (range 10))))
;;Returns [[9 7 5]]

;;Changing the log function so that it doesn't evaluate memo-result makes everything sane again
;;Log: [[clojure.lang.LazyTransformer 1] [clojure.lang.LazyTransformer 3] [clojure.lang.LazyTransformer 5] [clojure.lang.LazyTransformer 7] [clojure.lang.LazyTransformer 9]]
;;Okay, what is a clojure.lang.LazyTransformer?  Uh oh it's java code :(.  Fortunately transducers have been added to ClojureScript
;;so we can read ClojureScript source code.
(let [transducer (comp
                   (filter even?)
                   (map inc)
                   (logging-transducer #(println (type %1) %2))
                   (partition-by #(< % 5)))]
  (into [] (sequence transducer (range 10))))
;;Returns [[1 3] [5 7 9]]

;;How do we signal we want to halt all this crazyiness?  we can use reduced to do that!
;;This will stop a transducer whenever it hits the value x
(defn stopping-transducer [x]
  (fn [outer-transducer]
    (fn
      ([] (outer-transducer))
      ([memo-result] (outer-transducer memo-result))
      ([memo-result input]
         (if (= x input)
           (reduced memo-result)
           (outer-transducer memo-result input))))))
;;Lets try stopping in the same place we have been logging.
(let [transducer (comp
                   (filter even?)
                   (map inc)
                   (stopping-transducer 7)
                   (partition-by #(< % 5)))]
  (into [] transducer (range 10)))
;;Returns [[1 3] [5]]
;;The filter and map make [1 3 5 7 9] and our stopping-transducer stops everything when it hits 7

;;Now lets put the stopping-transducer at the very top
(let [transducer (comp
                   (stopping-transducer 7)
                   (filter even?)
                   (map inc)
                   (partition-by #(< % 5)))]
  (into [] transducer (range 10)))
;;Returns [[1 3] [5 7]]
;;We get the range (0 1 2 3 4 5 6 7 8 9) and the stopping-transducer halts everything after 7 which gives use
;;(0 1 2 3 4 5 6) which is filtered to (0 2 4 6) and then mapped to (1 3 5 7) and finally partitioned

;;We can combine it with our printing transducer to see what is going on
(let [transducer (comp
                   (stopping-transducer 7)
                   (filter even?)
                   (logging-transducer #(println %2)) ;;Only log the input
                   (map inc)
                   (partition-by #(< % 5)))]
  (into [] transducer (range 10)))
;;Returns [[1 3] [5 7]]
