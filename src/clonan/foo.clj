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
  (:refer-clojure :rename { print core-print }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IThing
  (to-char [_]))

(defprotocol IBoard
  (print [_]))

(def level-initial-states
     [ {} ;; levels are 1-based in ruby-warrior
      {:width  5, :rows [{ 0 warrior, 4 stairs }]}
      {:width 10, :rows [{ 0 warrior, 3 sludge, 9 stairs}] }
      ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype wall []
  IThing
  (to-char [_] \-))

(deftype warrior []
  IThing
  (to-char [_] \@))

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

(defn- border-row-to-string [width]
  (str \| (apply str (repeat width \-)) \|))

(defn- row-to-string [items]
  (let [chars (map to-char items)]
    (str \| (apply str chars) \|)))

;; pretty sure this is really missing the point of protocols, this is
;; not extensible

(defn- this-is-crap-code [cls]
  (condp = cls
      wall (wall.)
      warrior (warrior.)
      sludge (sludge.)
      stairs (stairs.)
      vacancy (vacancy.)))

(defn hydrate-row [width sparse-map]
  (map
   #(this-is-crap-code (get sparse-map % vacancy))
   (range width)))

(deftype board [content-width rows]
  IBoard
  (print [_]
         (println (border-row-to-string content-width))
         (doseq [row rows] (println (row-to-string row)))
         (println (border-row-to-string content-width))))

(defn make-board [level]
  (let [initial-state (level-initial-states level)
        width (:width initial-state)]
    (board. width
            (map #(hydrate-row width %) (:rows initial-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(let [level (level-initial-states 0)
      width (:width level)
      row (nth (:rows level) 0)]
  (pprint (hydrate-row width row))
  (println (row-to-string (hydrate-row width row)))
  )

(print (make-board 1))

