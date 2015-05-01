(ns markov-text.generator)

(require '[clojure.string :as str])

(def text "Jack and Jill went up a hill to fetch a pail of water. Jack fell down and broke his crown, and Jill came tumbling after.")

;;; Basic NLP
; todo: use OpenNLP to perform tokenization and to provide POS
; note that I will need a way to combine the POS when building the next phrase
; will need to know the POS of the next word also. It may ne worthwhile to
; use a Record for phrase and a record for word, to distinguish between them. but perhaps not.
; If i do use it, then a phrase can actually just be a vector of words. no need for the phrase to have
; its own POS. Or, I can use chunking, but that will likely be too sophisticated.
(defn tokenize
  "Given a string, break on whitespace and return tokens."
  [s]
  (str/split s #"\s+"))

(defn detokenize
  "Given a sequential collection of tokens, join by adding whitespace between then tokens."
  [words]
  (str/join " " words))

(defn phrase
  "basic textual unit, with text and part-of-speech"
  [text pos]
  {:text text :pos pos})

; todo: Add Prismatic/schema to check inputs
(defn phrase-id
  "Generates a unique identifier for each phrase."
  [p] (hash p))

;;; corpus is a set of phrases
;;; #{phrase1, phrase2, ..., phraseN}
;;;
;;; frequency table is a hash-map of phrase-id -> next-word-vector pairs
;;; {... phrase-id [word1 word2 ...]...}
;;;
;;; word has similar definition to phrase
;;; (defn word [text pos] {:text text :pos pos})

;(take 3 ["Jack" "and" "Jill" "went" "up" "a" "hill."])

;(build-phrase ["Jack" "and" "Jill" "went" "up" "a" "hill."])

(defn build-corpus
  [phrase-size tokens]
  (loop [phrases (transient #{})
         freqs (transient {})
         t tokens]
    (if (or (= (count t) 1) (empty? t))
      {:corpus (vec (persistent! phrases))
       :next-words (persistent! freqs)}
      (let [phrase-tokens (vec (take phrase-size t))
            next-token (last (take (+ 1 phrase-size) t))
            p (phrase phrase-tokens :nopos)
            id (phrase-id p)]
        (recur (conj! phrases p)
               (assoc! freqs id (conj (get id freqs []) next-token))
               (rest t))))))

(defn process-text
  [text]
  (let [doc text
        tokens (tokenize doc)]
    (build-corpus 2 tokens)))

;; corpus as defined like this is a vector
(def corpus  (:corpus (process-text text)))
(def freq-table (:next-words (process-text text)))

; todo change this from first to rand-nth when there's more training data
(defn- select-initial-phrase
  [corpus]
  (rand-nth corpus))

(defn- select-random-phrase
  [corpus]
  (rand-nth corpus))

(defn get-next-word
  "Given a phrase, select a word that follows the phrase in the corpus"
  [current-phrase freqs]
  (rand-nth (get freqs (phrase-id current-phrase))))

(defn generate-text-of-next-phrase
  "Given the current phrase and the next word, construct the next phrase."
  [current-phrase next-word]
  (let [current-text (:text current-phrase)
        current-text-minus-first-word  (vec (rest current-text))]
    (conj current-text-minus-first-word next-word)))

(defn find-phrase-in-corpus
  "Given the text of a phrase, find the phrase in the corpus.
  If there is more than one phrase, select one at random. Input text is a vector."
  [text corpus]
  (let [phrases (filter #(= (:text %) text) corpus)]
    (if (> (count phrases) 1)
      (rand-nth phrases)
      (first phrases))))

(defn get-next-phrase
  [current-phrase next-word corpus]
  (let [next-text (generate-text-of-next-phrase current-phrase next-word)]
    (find-phrase-in-corpus next-text corpus)))

(defn generate-sentence
  [num-words-in-sentence corpus freq-table]
  (let [current-phrase (atom (select-initial-phrase corpus))]
    (loop [sentence (transient [])
           iteration (range 0 num-words-in-sentence)]
      (if (empty? iteration)
        (persistent! sentence)
        (let [next-word (get-next-word (deref current-phrase) freq-table)
              next-phrase (get-next-phrase (deref current-phrase) next-word corpus)]
          (reset! current-phrase next-phrase)
          (recur (conj! sentence next-word)
                 (rest iteration)))))))

;(generate-sentence 5 corpus freq-table)