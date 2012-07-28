;;;; definitions
;;
;; table   : the probability distribution table
;; left    : left side of table, seed; is order tokens long
;; right   : right side of table, all options of what to grow to
;; pair    : a left and right to be trained
;; order   : the order of the markov chain; how far back to consider
;;
;; token   : minimal unit of growth
;; name  : a list of tokens from source material; has beginning and end
;; names : more than one name, i.e., more than one source

(def test-order 2)
(def test-names (map #(str % "\n") [ "Aeneas" "Amadeus" "Andreas" "Antonius" "Apollos" "Atticus" "Augustus" "Aurelius" "Caesar" "Caius" "Cassius" "Cato" "Cicero" "Claudius" "Cornelius" "Cosmo" "Cyrus" "Decimus" "Demetrius" "Felix" "Flavius" "Gaius" "Horatio" "Justus" "Lazarus" "Lucius" "Magnus" "Marcellus" "Marcus" "Marius" "Maximus" "Nero" "Octavius" "Philo" "Primus" "Quintus" "Remus" "Romanus" "Romulus" "Rufus" "Seneca" "Septimus" "Severus" "Stephanus" "Tarquin" "Theon" "Tiberius" "Titus" "Urban" ]))
(def test-start-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def test-end-chars ["\n"])

(defn train
  "Take a table and a pair, and return a table trained with that pair."
  [table [left right]]
  (update-in table [left] conj right))

(defn pairize
  "Take a name and an order, and return a list of all possible pairs."
  [name order]
  (for [chars (partition (inc order) 1 name)]
    [(butlast chars) (last chars)]))

(defn train-names
  "Create a probability distribution table based on list of names."
  [names order]
  (reduce (fn [table name]
            (reduce train table (pairize name order)))
          {}
          names))

(defn in?
  "Is n in coll?"
  [n coll] (some (partial = n) coll))

(defn starters
  "Return all starting pairs in table."
  [table start-chars]
  (for [[left right :as pair] table
        :when (in? (first left) start-chars)]
    left))

(defn done?
  "Return if name is complete."
  [name end-chars]
  (not (in? (last name) "abcdefghijklmnopqrstuvwxyz")))

; return random element of list
(def rnd (comp first shuffle))

; (load-file "markov.clj")

(defn build [table order start-chars end-chars]
  (let [start-rows (starters table start-chars)]
    (loop [word (apply str (rnd start-rows))]
      (let [letter (rnd (get table (take-last order word)))
            new-word (str word letter)]
        (if (done? new-word end-chars)
          new-word
          (recur new-word))))))

(def male-names (map #(str % "\n") (clojure.string/split (slurp "male_names.txt") #"\n")))

(dotimes [n 20]
  (println (apply str (butlast (build (train-names male-names 2)
       2 test-start-chars test-end-chars)))))

