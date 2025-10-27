(ns smp-labs.base-operation.base_function
  (:gen-class))

(defn no-repeat-strings [chars n]
  (if (zero? n)
    [""]
     (reduce
       (fn [acc s]
         (into acc
               (->> chars
                    (filter #(not= (last s) %))
                    (map #(str s %)))))
       []
       (no-repeat-strings chars (dec n)))))

(defn -main []
  (let [chars [\a \b \c]
        n 3
        results (no-repeat-strings chars n)]
    (println "Все строки длины" n "без повторов подряд:")
    (doseq [s results]
      (println s))))
