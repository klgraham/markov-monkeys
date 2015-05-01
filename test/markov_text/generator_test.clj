(ns markov-text.generator-test
  (:use midje.sweet)
  (:require [markov-text.generator :as mtg]))

(facts "Can perform basic NLP tasks."
       (fact "Can replace newlines with spaces."
             (let [text "An apple\n\na\nday."
                   expected "An apple a day."
                   newline-replaced (mtg/replace-newlines text)]
               (= expected newline-replaced)) => true)

       (fact "Tokenization and de-tokenization work as expected."
             (mtg/tokenize "Fear is the mind-killer.\n") => ["Fear" "is" "the" "mind-killer."]

             (mtg/detokenize ["Fear" "is" "the" "mind-killer."]) => "Fear is the mind-killer."))

(fact "Can generate a unique id for a phrase."
      (let [p1 {:text "John" :pos :NP}
            p2 {:text "john" :pos :NP}]
        (not= (mtg/phrase-id p1) (mtg/phrase-id p2))) => true)

(def corpus [{:text ["you" "?"], :pos ["PRP" "."]} {:text ["am" "i"], :pos ["VBP" "FW"]} {:text ["who" "are"], :pos ["WP" "VBP"]} {:text ["?" "who"], :pos ["." "WP"]} {:text ["are" "you"], :pos ["VBP" "PRP"]} {:text ["who" "am"], :pos ["WP" "VBP"]} {:text ["i" "?"], :pos ["FW" "."]}]
  )
(def freq-table {734084247 [{:text ["i"], :pos ["FW"]}], -154840124 [{:text ["?"], :pos ["."]}], -207278690 [{:text ["who"], :pos ["WP"]}], -1251257814 [{:text ["are"], :pos ["VBP"]}], 1781109736 [{:text ["you"], :pos ["PRP"]}], -1812499374 [{:text ["?"], :pos ["."]}], 155429507 [{:text ["?"], :pos ["."]}]}
  )

(facts "about sentence generation"
       (fact "Can generate the corpus and frequency table."
             (let [expected {:corpus corpus :next-words freq-table}
                   tokens (mtg/pos-tagger (mtg/tokenizer "who am i? who are you?"))
                   predicted (mtg/build-corpus 2 tokens)]
               (= expected predicted)) => true)

       (fact "Can get next word"
             (let [current-phrase {:text ["who" "am"], :pos ["WP" "VBP"]}
                   expected-next-word {:text ["i"], :pos ["FW"]}
                   predicted-next-word (mtg/get-next-word current-phrase freq-table)]
               (= expected-next-word predicted-next-word)) => true)

       (fact "Can get text of next phrase."
             (let [current-phrase {:text ["peaches" "are"] :pos ""}
                   next-word "awesome!"
                   expected ["are" "awesome!"]]
               (= expected (mtg/generate-text-of-next-phrase current-phrase next-word)) => true))

       (fact "Can find next-phrase in corpus, given its text"
             (let [text ["am" "i"]
                   expected {:text ["am" "i"], :pos ["VBP" "FW"]}]
               (= expected (mtg/find-phrase-in-corpus text corpus))) => true)

       (fact "Can get the next phrase from the corpus."
             (let [current-text {:text ["am" "i"], :pos ["VBP" "FW"]}
                   next-word "?"
                   expected {:text ["i" "?"], :pos ["FW" "."]}]
               (= expected (mtg/get-next-phrase current-text next-word corpus))) => true))
