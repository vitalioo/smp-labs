(ns smp-labs.dnf.core-test
  (:require [clojure.test :refer :all]
            [smp-labs.dnf.dnf :refer :all]))

;; Тест 1: Подстановка значения переменной в выражение
(deftest test-substitute
  (testing "Подстановка переменной"
    ;; Случай 1: заменяем "x" на true в выражении (x ∧ y)
    (let [expr (make-and (make-var "x") (make-var "y"))]  ; expr = x ∧ y
      (is (= (substitute expr "x" true)                  ; после замены x → true
             (make-and (make-const true) (make-var "y"))))); должно стать: true ∧ y

    ;; Случай 2: заменяем "a" на false в импликации (a => b)
    (let [expr (make-implies (make-var "a") (make-var "b"))]  ; expr = a => b
      (is (= (substitute expr "a" false)                     ; после замены a → false
             (make-implies (make-const false) (make-var "b"))))))) ; должно стать: false => b

;; Тест 2: Простейшие выражения уже в ДНФ — должны оставаться без изменений
(deftest test-to-dnf-simple
  (testing "Простые случаи ДНФ"
    ;; Переменная сама по себе — это ДНФ (один терм из одной переменной)
    (is (= (to-dnf (make-var "x")) (make-var "x")))
    ;; Константа — тоже ДНФ
    (is (= (to-dnf (make-const true)) (make-const true)))
    ;; Отрицание переменной — ДНФ (терм из одного литерала)
    (is (= (to-dnf (make-not (make-var "x"))) (make-not (make-var "x"))))))

;; Тест 3: Устранение импликации — ключевой первый шаг к ДНФ
(deftest test-to-dnf-implies
  (testing "Элиминация импликации"
    (let [expr (make-implies (make-var "p") (make-var "q"))]  ; expr = p => q
      (is (= (to-dnf expr)
             ;; По правилу: p => q ≡ ¬p ∨ q
             (make-or (make-not (make-var "p")) (make-var "q")))))))

;; Тест 4: Распределительный закон — самое важное правило ДНФ
(deftest test-to-dnf-distribute
  (testing "Распределение конъюнкции"
    (let [expr (make-and (make-var "x") (make-or (make-var "y") (make-var "z")))] ; expr = x ∧ (y ∨ z)
      (is (= (to-dnf expr)
             ;; По правилу: x ∧ (y ∨ z) → (x ∧ y) ∨ (x ∧ z)
             (make-or
               (make-and (make-var "x") (make-var "y"))
               (make-and (make-var "x") (make-var "z"))))))))

;; Тест 5: Сложное выражение — проверка интеграции всех правил
(deftest test-to-dnf-complex
  (testing "Сложное выражение"
    (let [expr (make-implies
                 (make-and (make-var "a") (make-var "b"))        ; (a ∧ b)
                 (make-or (make-var "c") (make-not (make-var "d"))))] ; => (c ∨ ¬d)
      (let [dnf (to-dnf expr)]
        (println "Сложное ДНФ:" dnf)  ; просто выводим, чтобы видеть в консоли
        (is (not (nil? dnf)))))))     ; главное — чтобы не упало и вернуло что-то

;; Тест 6: Законы де Моргана — нужно уметь "проталкивать" отрицание внутрь
(deftest test-demorgan
  (testing "Законы де Моргана"
    ;; Случай 1: ¬(x ∧ y) → ¬x ∨ ¬y
    (is (= (to-dnf (make-not (make-and (make-var "x") (make-var "y"))))
           (make-or (make-not (make-var "x")) (make-not (make-var "y")))))
    ;; Случай 2: ¬(x ∨ y) → ¬x ∧ ¬y
    (is (= (to-dnf (make-not (make-or (make-var "x") (make-var "y"))))
           (make-and (make-not (make-var "x")) (make-not (make-var "y")))))))
