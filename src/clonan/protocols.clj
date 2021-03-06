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

(in-ns 'clonan.core)

(defprotocol IThing
  (to-char [_])
  (empty? [_]))

(defprotocol IPlayer
  (play-turn [_ warrior]))

(defprotocol ILevelOneMethods
  (walk! [_] [_ direction]))

;; FIXME: this seems clumsy
(defprotocol ILevelTwoMethods
  (feel [_] [_ direction])
  (attack! [_] [_ direction])
  )

(defprotocol IBoard
  (print [_])
  (find [_ type])
  (get-warrior [_])
  (get-stairs [_])
  (transform [_ event])

  ;; warts below
  (content-width [_])
  (row [_])
  (respond-to-walk! [_]))

(defrecord Action [name direction])
