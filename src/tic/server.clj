(ns tic.server
  (:use [tic.tic :only [create-game print-help execute]]) 
  (:use [clojure.java.io :only [reader writer]]
        [server.socket :only [create-server]]))

; possible enhancements:
; save game state.
; play multiple games (i.e. accept multiple clients)

(defn- quit []
  (println "Quitting"))

; main logic here is heavely influenced by the fun clojure screencast
; by peepcode.  https://peepcode.com/products/functional-programming-with-clojure
(defn- main-game-loop [in out]
  (binding [*in* (reader in)
            *out* (writer out)
            *err* (writer System/err)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (println "\nWelcome to Tic-Tac-Toe")
    (print-help) (print "> ") (flush)

    (try (loop [input (read-line)]
      (when input
        (println (execute input))
        (.flush *err*)
        (print "> ") (flush))
        (recur (read-line)))
      (finally (quit)))))

(defn -main
  ([port-num]
     ; defonce is def + only once
     (defonce server (create-server (Integer. port-num) main-game-loop))
     (println "Launching Tic-tac-toe server on port: " port-num))
  ([] (-main 7623)))
