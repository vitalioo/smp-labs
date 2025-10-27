(ns smp-labs.base-operation.tale_recursion
  (:gen-class))

(defn generate-strings-tail
  "Хвостовая рекурсия: строки длины n без подряд совпадающих символов."
  [chars n]
  (letfn [(step [n prefix result]
            (if (= n 0)
              (conj result prefix)
              (reduce
                (fn [acc char]
                  (if (or (empty? prefix)
                          (not= (last prefix) char))
                    (step (dec n) (str prefix char) acc)
                    acc))
                result
                chars)))]
    (step n "" [])))

(defn -main
  [& args]
  (let [chars [\a \b]
        n 3
        results (generate-strings-tail chars n)]
    (println "Строки длины" n "без одинаковых символов подряд:")
    (println results)))
