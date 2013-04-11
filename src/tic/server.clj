(ns tic.server
  (:use [tic.tic :only [create-game print-help execute]]) 
  (:use [clojure.java.io :only [reader writer]]
        [server.socket :only [create-server]]))

; possible enhancements:
; save game state.
; play multiple games (i.e. accept multiple clients)

;(def ^:dynamic *game*)

;(def player-names (ref {}))
;(def current-games (ref {}))

(defn- cleanup []
  (println "Cleaning up"))

;(defn- cleanup []
  ;"Drop all inventory and remove player from room and player list."
  ;(dosync
   ;(doseq [item @*inventory*]
     ;(discard item))
   ;(commute player-streams dissoc *player-name*)
   ;(commute (:inhabitants @*current-room*)
            ;disj *player-name*)))

;(defn- get-unique-player-name [name]
  ;(if (@player-streams name)
    ;(do (print "That name is in use; try again: ")
        ;(flush)
        ;(recur (read-line)))
    ;name))
(defn- main-game-loop [in out]
  (binding [*in* (reader in)
            *out* (writer out)
            *err* (writer System/err)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print "\nWhat is your name? ") (flush)
    ; add name to the create game
    (let [player-name (read-line) game nil]
      (println "\nWelcome " player-name)
      (print-help) (print "> ") (flush)

      (try (loop [input (read-line)]
             (when input
               (let [game (execute game input)]
                 (.flush *err*)
                 (println "Game: " game)
                 (print "> ") (flush))
               (recur (read-line))))
           (finally (cleanup))))))

(defn -main
  ([port-num]
     ; defonce is def + only once
     (defonce server (create-server (Integer. port-num) main-game-loop))
     (println "Launching Tic-tac-toe server on port: " port-num))
  ([] (-main 7623)))
