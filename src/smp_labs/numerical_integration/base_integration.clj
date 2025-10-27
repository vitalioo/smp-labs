(ns smp-labs.numerical-integration.base_integration
  (:gen-class))

(defn trapezoid-integral [f h]
  (fn [x]
    (let [n (int (/ x h))                      ; считаем количество трапеций
          xs (map #(* % h) (range (inc n)))    ; список всех точек по оси x
          ys (map f xs)                        ; считаем значения функции в каждой точке
          areas (map (fn [y1 y2]
                       (* 0.5 h (+ y1 y2)))
                     ys (rest ys))]
      (reduce + 0 areas))))

(defn -main [& args]
  (let [f #(* %)
        h 0.001
        integrator (trapezoid-integral f h)]
    (time
      (println "Trapezoid integral of x from 0 to 5:" (integrator 100.0)))))