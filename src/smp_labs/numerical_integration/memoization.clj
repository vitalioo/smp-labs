(ns smp-labs.numerical-integration.memoization
  (:gen-class))

(defn trapezoid-integral [f h]
  (let [integrate
        (fn [x]
          (let [n (int (/ x h))
                xs (map #(* % h) (range (inc n)))
                ys (map f xs)
                areas (map (fn [y1 y2]
                             (* 0.5 h (+ y1 y2)))
                           ys (rest ys))]
            (reduce + 0 areas)))
        memo-integrate (memoize integrate)]  ;; мемоизируем функцию интеграла
    memo-integrate))

(defn -main [& args]
  (let [f #(* %)
        h 0.001
        integrator (trapezoid-integral f h)]
    ;; первый вызов
    (time (println "Memoized trapezoid integral at 100.0:" (integrator 100.0)))
    ;; повторные вызовы
    (time
      (dotimes [_ 10]
        (println "Memoized (cached) trapezoid integral at 100.0:" (integrator 100.0))))))
