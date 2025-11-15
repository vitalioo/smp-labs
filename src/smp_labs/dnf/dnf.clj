(ns smp-labs.dnf.dnf
  "Символьные булевы выражения и приведение к ДНФ.

  Основные функции:
  - `make-var`, `make-const` — создание переменных и констант
  - `make-and`, `make-or`, `make-not`, `make-implies` — конструкторы операций
  - `substitute [expr var-name value]` — подстановка значения переменной
  - `to-dnf [expr]` — приведение выражения к ДНФ
  Расширение:
  Чтобы добавить новую операцию, определите мультиметод для `to-dnf`:
  (defmethod to-dnf :new-op [expr] ...)
  "
  (:require [clojure.string :as str]))

;; ===== 1. Представление выражений =====
(defn make-const [value] [:const (boolean value)]) ;; Пример: (make-const true) → [:const true]

(defn make-var [name] [:var (str name)]) ;; Пример: (make-var "x") → [:var "x"]

(defn make-and [& args] (into [:and] (filter #(not= [:const true] %) args)))
;; Пример: (make-and x (make-const true) y) → [:and x y]  ; true убирается

(defn make-or [& args] (into [:or] (filter #(not= [:const false] %) args)))
;; Пример: (make-or x (make-const false) y) → [:or x y]  ; false убирается

(defn make-not [expr] [:not expr]) ;; Пример: (make-not (make-var "x")) → [:not [:var "x"]]

(defn make-implies [a b] [:implies a b])
;; Пример: (make-implies (make-var "x") (make-var "y")) → [:implies [:var "x"] [:var "y"]]

;; ===== 2. Подстановка значения =====
(defn substitute [expr var-name value]
  (cond
    (= (first expr) :var) (if (= (second expr) (str var-name))
                            (make-const value)
                            expr)
    (= (first expr) :not) (make-not (substitute (second expr) var-name value))
    (#{:and :or :implies} (first expr))
    (let [op (first expr)
          args (map #(substitute % var-name value) (rest expr))]
      (case op
        :and (apply make-and args)
        :or (apply make-or args)
        :implies (make-implies (first args) (second args))))
    :else expr))                                            ; x ∧ (y ∨ ¬z) -> true ∧ (y ∨ ¬z) = y ∨ ¬z

;; ===== 3. Вспомогательные функции для упрощения =====
(defn apply-demorgan [expr]
  (if (= :not (first expr))
    (let [arg (second expr)]
      (cond
        (= :and (first arg)) (apply make-or (map #(make-not %) (rest arg))) ; ¬(a ∧ b) → ¬a ∨ ¬b
        (= :or (first arg)) (apply make-and (map #(make-not %) (rest arg))) ; ¬(a ∨ b) → ¬a ∧ ¬b
        (= :not (first arg)) (second arg) ; двойное отрицание
        :else expr))
    expr))

(defn distribute-and [expr]                                 ; a ∧ (b ∨ c) → (a ∧ b) ∨ (a ∧ c)
  (if (= :and (first expr))
    (let [args (rest expr)
          or-args (filter #(= :or (first %)) args)
          other-args (filter #(not= :or (first %)) args)]
      (if (seq or-args)
        (let [first-or (first or-args)
              rest-or (rest or-args)]
          (apply make-or
                 (for [term (rest first-or)]
                   (distribute-and (apply make-and (concat other-args [term] rest-or))))))
        expr))
    expr))

;; ===== 4. Приведение к ДНФ (через мультиметоды) =====
(defmulti to-dnf (fn [expr] (first expr)))

(defmethod to-dnf :const [expr] expr)
(defmethod to-dnf :var [expr] expr)

(defmethod to-dnf :not [expr]
  (let [arg-dnf (to-dnf (second expr))]
    (apply-demorgan (make-not arg-dnf))))

(defmethod to-dnf :and [expr]
  (let [args-dnf (map to-dnf (rest expr))
        simplified (apply make-and args-dnf)]
    (distribute-and simplified)))

(defmethod to-dnf :or [expr]
  (let [args-dnf (map to-dnf (rest expr))]
    (apply make-or args-dnf)))

(defmethod to-dnf :implies [expr]
  (to-dnf (make-or (make-not (second expr)) (nth expr 2))))

;; ===== 5. Пример использования в main =====
(defn -main []
  (let [expr (make-implies (make-var "x") (make-or (make-var "y") (make-var "z")))] ; x -> (y or z) = true -> (y or z) =
    (println "Исходное выражение:" expr)
    (println "После подстановки x=true:" (substitute expr "x" true))
    (println "ДНФ:" (to-dnf expr))))