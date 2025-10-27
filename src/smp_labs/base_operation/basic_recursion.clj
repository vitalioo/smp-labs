(ns smp-labs.base-operation.basic-recursion
  (:gen-class))

(defn generate-strings
  "Генерирует все строки длины n из символов chars,
   исключая строки с повторяющимися подряд символами."
  [chars n & [prefix]]
  (let [prefix (or prefix "")]
    (if (= n 0)
      [prefix]
      (apply concat
             (for [char chars
                   :let [new-prefix (str prefix char)]
                   :when (or (empty? prefix)
                             (not= (last prefix) char))]
               (generate-strings chars (dec n) new-prefix))))))

(defn -main
  [& args]
  (let [chars [\a \b \c]
        n 2
        results (generate-strings chars n)]
    (println "Все строки длины" n "без повторяющихся подряд символов:")
    (doseq [s results]
      (println s))))
