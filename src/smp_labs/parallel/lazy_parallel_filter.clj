(ns smp-labs.parallel.lazy-parallel-filter
  (:gen-class))

(defn lazy-parallel-filter [pred coll n-block]
  (letfn [(process-block [block]
            (future (doall (filter pred block))))

          (filter-blocks [coll]
            (lazy-seq
              (when (seq coll)
                (let [block (take n-block coll)
                      future-result (process-block block)
                      rest-coll (drop n-block coll)]
                  (concat @future-result
                          (filter-blocks rest-coll))))))]
    (filter-blocks coll)))

(defn busy-pred [x]
  (let [limit 5000]
    (loop [i 0, acc x]
      (if (< i limit)
        (recur (inc i) (bit-xor acc (+ (* i 123) (bit-and acc 65535))))
        (odd? acc)))))

(defn -main [& args]
  (let [
        infinite-data (range)
        pred busy-pred
        n-block 1000
        ;; запросим первые 40000 элементов (из бесконечной последовательности)
        n-elements 40000]

    (println "SEQUENTIAL (take" n-elements ")...")
    (time
      (let [res (doall (take n-elements (filter pred infinite-data)))]
        (println "Count:" (count res))))

    (println "LAZY PARALLEL (take" n-elements ")...")
    (time
      (let [res (doall (take n-elements (lazy-parallel-filter pred infinite-data n-block)))]
        (println "Count:" (count res))))))