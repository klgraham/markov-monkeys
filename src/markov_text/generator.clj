(ns markov-text.generator)

(require '[clojure.string :as str])
(require '[opennlp.nlp :as nlp])
(use 'opennlp.treebank)
(use 'clojure.pprint)

(def text "John sees spot run. John sees spot jump. John sees spot swim.")

;;; Basic NLP
; todo: use OpenNLP to perform tokenization and to provide POS
; note that I will need a way to combine the POS when building the next phrase
; will need to know the POS of the next word also. It may ne worthwhile to
; use a Record for phrase and a record for word, to distinguish between them. but perhaps not.
; If i do use it, then a phrase can actually just be a vector of words. no need for the phrase to have
; its own POS. Or, I can use chunking, but that will likely be too sophisticated.

(defn replace-newlines
  [s]
  (str/replace s #"\n+" " "))

(defn tokenize
  "Given a string, break on whitespace and return tokens."
  [s]
  (-> s replace-newlines str/trim (str/split #"\s+")))

(defn detokenize
  "Given a sequential collection of tokens, join by adding whitespace between then tokens."
  [words]
  (str/join " " words))

;;; more advanced NLP, from OpenNLP

(def get-sentences (nlp/make-sentence-detector "resources/models/en-sent.bin"))
(def tokenizer (nlp/make-tokenizer "resources/models/en-token.bin"))
(def detokenizer (nlp/make-detokenizer "resources/models/english-detokenizer.xml"))
(def pos-tagger (nlp/make-pos-tagger "resources/models/en-pos-perceptron.bin"))
;(def pos-tagger (nlp/make-pos-tagger "resources/models/en-pos-maxent.bin"))
(def name-finder (nlp/make-name-finder "resources/models/en-ner-person.bin"))
(def location-finder (nlp/make-name-finder "resources/models/en-ner-location.bin"))
(def org-finder (nlp/make-name-finder "resources/models/en-ner-organization.bin"))
(def chunker (make-treebank-chunker "resources/models/en-chunker.bin"))

;;(pos-tagger (tokenizer text)) => (["see" "VB"] ["spot" "NN"] ["run" "NN"] ["." "."] ["see" "VB"] ["spot" "NN"] ["jump" "NN"] ["." "."] ["see" "VB"] ["spot" "NN"] ["swim" "NN"] ["." "."])
;; need to turn each pair [text pos] into a hashmap

(defn phrase
  "basic textual unit, with text and part-of-speech"
  [text pos]
  {:text text :pos pos})

(defn token-and-pos->phrase
  [pair]
  (phrase (vector (first pair)) (vector (second pair))))

(defn make-composite-phrase
  [seq-of-text-pos-pairs]
  (loop [phrase (transient {})
        pairs seq-of-text-pos-pairs]
   (if (empty? pairs)
     (persistent! phrase)
     (let [[text pos] (first pairs)]
       (recur (assoc! phrase :text (vec (conj (get phrase :text) text)) :pos (vec (conj (get phrase :pos) pos)))
              (rest pairs))))))

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

; todo: frequency table has tons of repeats for large docs. need to make a real freq table and then sample from it.
(defn build-corpus
  [phrase-size tokens-and-pos]
  (loop [phrases (transient #{})
         freqs (transient {})
         t tokens-and-pos]
    (if (or (= (count t) 1) (empty? t))
      {:corpus (vec (persistent! phrases))
       :next-words (persistent! freqs)}
      (let [raw-phrase (take phrase-size t)
            next-token-and-pos (first (rseq (vec (take (+ 1 phrase-size) t))))
            p (make-composite-phrase raw-phrase)
            id (phrase-id p)]
        ;(println raw-phrase)
        ;(println next-token-and-pos)
        ;(println p)
        (recur (conj! phrases p)
               (assoc! freqs id (conj (get freqs id []) (token-and-pos->phrase next-token-and-pos)))
               (rest t))))))

(defn process-text
  [text phrase-size]
  (let [doc text
        tokens-and-pos (pos-tagger (tokenizer doc))]
    (build-corpus phrase-size tokens-and-pos)))

; todo: limit the initial phrase by POS. Perhaps only nouns and adj? Can experiment.
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

; todo: append period to end of sentence
(defn generate-sentence
  "Given the desired sentence length, the corpus, and the frequency table, generate a sentence.
  Returns a vector"
  [num-words-in-sentence corpus freq-table]
  (let [current-phrase (atom (select-initial-phrase corpus))]
    (loop [sentence (transient [])
           iteration (range 0 num-words-in-sentence)]
      (if (empty? iteration)
        (detokenize (persistent! sentence))
        (let [next-word (first (:text (get-next-word (deref current-phrase) freq-table)))
              next-phrase (get-next-phrase (deref current-phrase) next-word corpus)]
          (reset! current-phrase next-phrase)
          (recur (conj! sentence next-word)
                 (rest iteration)))))))

;; corpus as defined like this is a vector
(def phrase-size 3)
(def processed-text (process-text text phrase-size))
(def corpus  (:corpus processed-text))
(def freq-table (:next-words processed-text))
(generate-sentence 10 corpus freq-table)