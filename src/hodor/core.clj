(ns hodor.core
  (:require
    [botty.core :as botty]
    [clojure.pprint :refer [pprint]]
    [clojure.tools.reader.edn :as edn])
  (:gen-class))

(comment
  "I will reinstate the message checking real soon now..."
  (def throttle-window 20000) ; milliseconds
  (def throttle-state (atom {:throttling false
                             :hodors     #{}})) ; set of hodor timestamps
  (def max-hodors 5)
  (def dethrottle-hodors 3)

  (defn throttling? []
    (:throttling @throttle-state))

  (defn update! [{:keys [throttling hodors]}]
    (let [now (System/currentTimeMillis)
          threshold (- now throttle-window)
          pruned-hodors (clojure.set/select (partial < threshold) hodors)
          hodor-count (count pruned-hodors)
          over-max (> hodor-count max-hodors)
          under-min (< hodor-count dethrottle-hodors)
          new-throttling (if throttling
                           (if under-min false true)
                           (if over-max true false))
          _ (println "hodor count" hodor-count "was-t:" throttling "now-t:" new-throttling)]
      {:hodors     pruned-hodors
       :throttling new-throttling}))

  (defn new-hodor! [{:keys [throttling hodors]}]
    {:throttling throttling
     :hodors     (conj hodors (System/currentTimeMillis))})

  (defn send-hodor [irc target]
    (let [was-throttling (throttling?)
          _ (swap! throttle-state update!)
          throttling (throttling?)]
      (if throttling
        (when-not was-throttling
          (irclj/ctcp irc target :action "is confused"))
        (do
          (swap! throttle-state new-hodor!)
          (irclj/message irc target "hodor")))))

  (defn has-hodor? [text]
    (re-find #"(?i)(\W|^)hodor(\W|$)" text))
  )

(defn on-tick [world]
  (botty/broadcast-message! world "hodor...")
  world)

(defn on-command [world {:keys [type value from-nick reply-to] :as payload}]
  (case type
    :command
    (do (case (clojure.string/trim value)
      "quit" (do
               (botty/broadcast-message! world "Hodor!")
               (botty/quit! world "Hodor!"))
      "yell" (botty/broadcast-message! world "HODOR!!")
      "whisper" (botty/broadcast-message! world "(hodor)")
      (botty/send-message! world reply-to "hodor?"))
    world)
    :match
    (do
      (botty/broadcast-message! world "Hodor")
      world)))

(def default-config
  {:botname  "hodor"
   :channels ["#general"]
   :irchost  "localhost"
   :ircport  6667
   :tick-ms  60000
   :matcher "(?i).*(^|\\W)hodor(\\W|$).*"
   }
  )

(defn -main [& args]
  (let [config (case (count args)
                 0 default-config
                 1 (edn/read-string (slurp (first args)))
                 (throw (Exception. "please specify a config file, or no args for dev-only defaults")))
        wait-for-death-fn (botty/irc-loop config {:on-tick on-tick
                                       :on-command on-command})]
    (prn "Hodor...")
    (wait-for-death-fn)
    (prn "hodor!")))
