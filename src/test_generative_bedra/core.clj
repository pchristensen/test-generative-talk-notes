(ns test-generative-bedra.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.data :as data]
            [clojure.test.generative.generators :as gen]))

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
