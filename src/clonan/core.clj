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

(ns clonan.core
  (:refer-clojure :rename { print core-print, find core-find })
  (:use [clojure.contrib.seq :only [positions]]))

(load "protocols")
(load "util")
(load "types")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def level-initial-states
     [ {} ;; levels are 1-based in ruby-warrior
       {:width  8, :rows [{ 0 warrior, 7 stairs }]}
       ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pretty sure this is really missing the point of protocols, this is
;; not extensible
(defn- entity-factory [cls]
  (condp = cls
      wall (wall.)
      warrior (warrior. (atom nil))
      sludge (sludge.)
      stairs (stairs.)
      vacancy (vacancy.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hydrate-row [width position-entity-map]
  (apply vector (map #(entity-factory (get position-entity-map % vacancy))
                     (range width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- take-action [warrior player]
  (play-turn player warrior)
  @(:last-action warrior))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-board [level]
  (let [initial-state (level-initial-states level)
        width (:width initial-state)
        rows (map #(hydrate-row width %) (:rows initial-state))]
    (board. width (apply vector rows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn play-level [level player]
  (println "Starting level " level)
  (loop [board (make-board level) turn 1]
    (let [warrior-pos (find board warrior)
          stair-pos (find board stairs)]
      (println "- turn" turn "-")
      (print board)
      (if (or (nil? stair-pos) (>= warrior-pos stair-pos))
        (println "Success! You have found the stairs.")
        (recur (transform board (take-action (get-warrior board) player)) (inc turn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (print (make-board 1))
;; (find (make-board 1) warrior)
(play-level 1 (player.))
