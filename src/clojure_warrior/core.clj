(ns clojure-warrior.core)

(defprotocol warrior-level-one
  (walk! [_]))

(deftype respond-to-level-one [board]
  warrior-level-one
  (walk! [_] (println "Playing over here!")))

(def levels [{ :responder respond-to-level-one
              :board-width 4
              :board { 0 :warrior, 3 :stairs }}
             { :responder respond-to-level-one
              :board-width 10
              :board { 0 :warrior, 5 :sludge, 9 :stairs }}
             ])

(defn generate-board [level-num]
  (let [level (levels level-num)]
    (reduce
     (fn [board idx] (conj board
                          (get (:board level) idx :empty))) 
       []
       (range (:board-width level)))))

(defn print-space [symbol]
  (let [known-pieces { :warrior "@" :stairs ">" :sludge "s" }]
    (get known-pieces symbol " ")))

(generate-board 0)

(defn play-level-one [warrior]
  (println "I'm playing!")
  (walk! warrior))

(defn print-board [board]
  (let [wall (apply str (repeat (+ 2 (count board)) "-"))
        floor (map #(print-space %) board)]
  (println wall)
  (println (str "|"  (apply str floor) "|"))
  (println wall)))

(defn run-game [level-num player-logic]
  (let [level (levels level-num)
        board (generate-board level-num)
        ;; responder ((:responder level) )
        ;; warrior (reify level-protocol
        ;;                (walk! [_] (println "I walked!")))
        ]
    (print-board board)))

(run-game 0 play-level-one)
