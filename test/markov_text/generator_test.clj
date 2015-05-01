(ns markov-text.generator-test
  (:use midje.sweet)
  (:require [markov-text.generator :as mtg]))

(fact "Tokenization and de-tokenization work as expected."
      (mtg/tokenize "Fear is the mind-killer.") => ["Fear" "is" "the" "mind-killer."]

      (mtg/detokenize ["Fear" "is" "the" "mind-killer."]) => "Fear is the mind-killer.")

(fact "Can generate a unique id for a phrase."
      (let [p1 {:text "John" :pos :NP}
            p2 {:text "john" :pos :NP}]
        (not= (mtg/phrase-id p1) (mtg/phrase-id p2))) => true)

(def corpus [{:text ["am" "i?"], :pos :nopos} {:text ["who" "am"], :pos :nopos} {:text ["i?" "who"], :pos :nopos} {:text ["are" "you?"], :pos :nopos} {:text ["who" "are"], :pos :nopos}])
(def freq-table {665315662 ["i?"], -281269630 ["who"], 2119923378 ["are"], 588047507 ["you?"], -127521741 ["you?"]})

(facts "about sentence generation"
       (fact "Can generate the corpus and frequency table."
             (let [expected {:corpus corpus :next-words freq-table}
                   tokens (mtg/tokenize "who am i? who are you?")
                   predicted (mtg/build-corpus 2 tokens)]
               (= expected predicted)) => true)

       (fact "Can get next word"
             (let [current-phrase {:text ["who" "am"], :pos :nopos}
                   expected-next-word "i?"
                   predicted-next-word (mtg/get-next-word current-phrase freq-table)]
               (= expected-next-word predicted-next-word)) => true)

       (fact "Can get text of next phrase."
             (let [current-phrase {:text ["peaches" "are"] :pos ""}
                   next-word "awesome!"
                   expected ["are" "awesome!"]]
               (= expected (mtg/generate-text-of-next-phrase current-phrase next-word)) => true))

       (fact "Can find next-phrase in corpus, given its text"
             (let [text ["am" "i?"]
                   expected {:text ["am" "i?"], :pos :nopos}]
               (= expected (mtg/find-phrase-in-corpus text corpus))) => true)

       (fact "Can get the next phrase from the corpus."
             (let [current-text {:text ["am" "i?"], :pos :nopos}
                   next-word "who"
                   expected {:text ["i?" "who"], :pos :nopos}]
               (= expected (mtg/get-next-phrase current-text next-word corpus))) => true))
