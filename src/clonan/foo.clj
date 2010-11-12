;;  This file is part of clojure-warrior.
;;
;;  clojure-warrior is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  clojure-warrior is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with clojure-warrior.  If not, see <http://www.gnu.org/licenses/>.

(ns clonan.foo
  (:refer-clojure :rename { print core-print, find core-find })
  (:use [clojure.contrib.seq :only [positions]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IThing
  (to-char [_]))

(defprotocol IBoard
  (print [_])
  (find [_ type])
  (get-warrior [_])
  (get-stairs [_])
  (transform [_ event])

  ;; warts below
  (content-width [_])
  (row [_]))

(defprotocol IPlayer
  (play-turn [_ warrior]))

(defprotocol ILevelOneMethods
  (walk! [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype player []
  IPlayer
  (play-turn [_ clonan] (walk! clonan)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype wall []
  IThing
  (to-char [_] \-))

(defrecord warrior [last-action]
  IThing
  (to-char [_] \@)

  ILevelOneMethods
  (walk! [_] (reset! last-action :walk!)))

;; (def w (warrior. (atom nil)))

;; @(:last-action w)
;; (walk! w)

(deftype sludge []
  IThing
  (to-char [_] \s))

(deftype stairs []
  IThing
  (to-char [_] \>))

(deftype vacancy []
  IThing
  (to-char [_] \space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def level-initial-states
     [ {} ;; levels are 1-based in ruby-warrior
       {:width  5, :rows [{ 0 warrior, 4 stairs }]}
       {:width 10, :rows [{ 0 warrior, 3 sludge, 9 stairs}] }
       ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- border-row-to-string [width]
  (str \| (apply str (repeat width \-)) \|))

(defn- row-to-string [items]
  (let [chars (map to-char items)]
    (str \| (apply str chars) \|)))

(deftype board [width rows]
  IBoard
  
  (print [_]
    (let [wall (border-row-to-string width)]
      (println wall)
      (doseq [row rows] (println (row-to-string row)))
      (println wall)))

  (find [_ expected-type]
    (first (positions #(= expected-type (class %)) (first rows))))

  (get-warrior [self]
    (nth (row self) (find self warrior)))

  (get-stairs [self]
    (nth (row self) (find self stairs)))

  (transform [self event]
    (let [clonan (get-warrior self)
          clonan-pos (find self warrior)]
    (if (= event :walk!)
      (board. (content-width self) (vector (assoc (row self) clonan-pos (vacancy.), (inc clonan-pos) clonan)))
      (throw (Exception. (str "unexpected action of " event))))))
  
  ;; warts
  (content-width [_] width)
  (row [_] (first rows)))

;; pretty sure this is really missing the point of protocols, this is
;; not extensible

(defn- entity-factory [cls]
  (condp = cls
      wall (wall.)
      warrior (warrior. (atom nil))
      sludge (sludge.)
      stairs (stairs.)
      vacancy (vacancy.)))

(defn- hydrate-row [width position-entity-map]
  (apply vector (map #(entity-factory (get position-entity-map % vacancy))
                     (range width))))

(defn make-board [level]
  (let [initial-state (level-initial-states level)
        width (:width initial-state)
        rows (map #(hydrate-row width %) (:rows initial-state))]
    (board. width (apply vector rows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- take-action [warrior player]
  (play-turn player warrior)
  @(:last-action warrior))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn play-level [level player]
  (loop [board (make-board level)]
    (let [warrior-pos (find board warrior)
          stair-pos (find board stairs)]
      (print board)
      (if (or (nil? stair-pos) (>= warrior-pos stair-pos))
        (println "you win!")
        (recur (transform board (take-action (get-warrior board) player)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(play-level 1 (player.))
