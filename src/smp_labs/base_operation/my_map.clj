(ns smp-labs.base-operation.my_map
  (:gen-class))

(defn my-map [f coll]
  (reduce
    (fn [acc el]
      (conj acc (f el)))
    []
    coll))

(defn my-filter [pred coll]
  (reduce
    (fn [acc el]
      (if (pred el)
        (conj acc el)
        acc))
    []
    coll))

(defn -main
  [& args]
  (let [numbers [1 2 3 4 5 6]
        mapped (my-map dec numbers)
        filtered (my-filter odd? numbers)]
    (println "my-map (inc) applied to" numbers ":")
    (println mapped)
    (println "my-filter (odd?) applied to" numbers ":")
    (println filtered)))
