(ns smp-labs.numerical-integration.partial_sums_integration
  (:gen-class))

(defn partial-sums-trapezoid [f h]
  (let [xs (map #(* % h) (range))
        ys (map f xs)
        trapezoids (map (fn [y1 y2] (* 0.5 h (+ y1 y2))) ys (rest ys))]
    ;; ленивые частичные суммы
    (reductions + 0 trapezoids)))

(defn trapezoid-integral [f h]
  (let [partial-sums (partial-sums-trapezoid f h)]
    (fn [x]
      (let [idx (int (/ x h))]
        ;; выбираем частичную сумму для нужного индекса,
        (nth partial-sums idx)))))

(defn -main [& args]
  (let [f #(* %)
        h 0.001
        integrator (trapezoid-integral f h)]
    ;; фаза вычисления кеша
    (time (println "Trapezoid integral at 10000.0:" (integrator 10000.0)))
    (time (println "Cached trapezoid integral at 5.0:" (integrator 5.0)))
    (time (println "Cached trapezoid integral at 7000.0:" (integrator 7000.0)))
    (time (println "Cached trapezoid integral at 11000.0:" (integrator 11000.0)))
    (time (println "Cached trapezoid integral at 500.0:" (integrator 500.0)))))
