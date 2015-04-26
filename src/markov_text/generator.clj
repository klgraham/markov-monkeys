(ns markov-text.generator)

(def text "Jack and Jill went up a hill to fetch a pail of water. Jack fell down and broke his crown, and Jill came tumbling after.")

(defn phrase
  ([text]
   {:text text :pos "" :id (hash (str text "_"))})

  ([text pos]
   {:text text :pos pos :id (hash (str text "_" pos))}))

;;; corpus will look like
;;; [phrase1, phrase2, ..., phraseN]
;;;
;;; frequency table will look like:
;;; {... phrase-id [word1 word2 ...]...}
;;;
;;; word has similar definition to phrase
;;; (defn word [text pos] {:text text :pos pos})

(require '[clojure.string :as str])

(defn build-phrase
  [words]
  (str/join " " words))

;(take 3 ["Jack" "and" "Jill" "went" "up" "a" "hill."])

;(build-phrase ["Jack" "and" "Jill" "went" "up" "a" "hill."])

(defn build-corpus
  [phrase-size tokens]
  (loop [phrases (transient [])
         freqs (transient {})
         t tokens]
    (if (empty? t)
      {:corpus (persistent! phrases)
       :next-words (persistent! freqs)}
      (let [phrase-tokens (build-phrase (take phrase-size t))
            next-token (last (take (+ 1 phrase-size) t))
            p (phrase phrase-tokens)]
        (recur (conj! phrases p)
               (assoc! freqs (:id p) (conj (get (:id p) freqs []) next-token))
               (rest t))))))

(defn process-text
  [text]
  (let [doc text
        tokens (str/split doc #"\s+")]
    (build-corpus 2 tokens)))

;; todo remove duplicates from corpus
(def corpus  (:corpus (process-text text)))
(def freq-table (:next-words (process-text text)))

(defn select-first-word
  [corpus]
  (:text (rand-nth corpus) "NONE"))

(select-first-word corpus)

(defn get-next-word
  [current-phrase freqs]
  (let [phrase (:id current-phrase)]
    (rand-nth (get freqs phrase))))

;; todo turn phrase into a Record
(defn get-next-phrase
  [current-phrase freqs corpus]
  (let [next-word (get-next-word current-phrase freqs)
        current-text (:text current-phrase)
        current-text-minus-first-word (rest (str/split current-text #" "))
        next-text (str/trimr (str/join " " (conj (vec current-text-minus-first-word) next-word)))
        text-key (hash next-text)
        next-phrase (filter #(= (:text %) next-text) corpus)]
    (println current-phrase)
    (println next-word)
    (println current-text)
    (println next-text)
    (println text-key)
    next-phrase))

(def c (rand-nth corpus))

(get-next-phrase c freq-table corpus)