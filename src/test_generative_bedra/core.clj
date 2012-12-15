(ns test-generative-bedra.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.data :as data]
            [clojure.test.generative.generators :as gen])
  (:use [clojure.test.generative :only (defspec) :as test]))

(defn exact-matches
  "Given two collections, return the number of positions where the collections contain equal items."
  [c1 c2]
  (let [[_ _ matches] (data/diff c1 c2)]
    (count (remove nil? matches))))

(defn unordered-matches
  "Given two collections, return a map where each key is an item in both collections, and each value is the minumum number of occurences."
  [c1 c2]
  (let [f1 (select-keys (frequencies c1) c2)
        f2 (select-keys (frequencies c2) c1)]
    (merge-with min f1 f2)))

(defn score [c1 c2]
  (let [exact (exact-matches c1 c2)
        unordered (apply +
                         (vals
                          (unordered-matches c1 c2)))]
    {:exact exact :unordered (- unordered exact)}))

(defn generate-turn-inputs
  "Generate all possible turn inputs for a clojurebreaker game with colors and n columns"
  [colors n]
  (-> (comb/selections colors 2)
      (comb/selections 2)))

(defn score-inputs
  "Given a sequence of turn inputs, return a lazy sequence of maps with :secret, :guess, and :score."
  [inputs]
  (map
   (fn [[secret guess]]
     {:secret (seq secret)
      :guess (seq guess)
      :score (score secret guess)})
   inputs))

;; Display all combinations
; (->> (generate-turn-inputs [:r :g :b] 2) (score-inputs))

;; Print the entire domain to a file
;; (use 'clojure.pprint)
;; (require '[clojure.java.io :as io])
;; (with-open [w (io/writer "scoring-table")]
;;   (binding [*out* w]
;;     (print-table
;;      (->> (generate-turn-inputs [:r :g :b :y] 4)
;;           (score-inputs)))))

(defn random-secret []
  (gen/vec #(gen/one-of :r :g :b :y) 4))

;; 9:00 Define the system constraints/contracts
(defn matches
  [score]
  (+ (:exact score) (:unordered score)))

(defn scoring-is-symmetric
  [secret guess sc]
  (= sc (score guess secret)))

(defn scoring-is-bounded-by-number-of-pegs
  [secret guess score]
  (<= 0 (matches score) (count secret)))

(defn reordering-the-guess-does-not-change-matches
  [secret guess sc]
  (= #{(matches sc)}
     #{(matches sc)})) ;; This function was cutoff on screen so I wrote a tautology

;; 9:30 Try out contracts with simple data
;; 10:00 Create a test.generative test

(defspec score-invariants
  score
  [^{:tag `random-secret} secret
   ^{:tag `random-secret} guess]
  (assert (scoring-is-symmetric secret guess %))
  (assert (scoring-is-bounded-by-number-of-pegs secret guess %)))

;; ;; 13:00 Practical cases - testing Clojure numerics
;; (defspec integer-commutative-laws
;;   (partial map identity)
;;   [^{:tag `integer} a ^{:tag `integer} b]
;;   (if (longable? (+' a b))
;;     (assert (= (+ a b) (+ b a)
;;                (+' a b) (+' b a)
;;                (unchecked-add a b) (unchecked-add b a)))
;;     (assert (= (+' a b) (+' b a)))))