(ns hodor.core
  (:require [irclj.core :as irclj]
            [clojure.pprint :refer [pprint]]))

(defn raw-log [_ & args]
  (pprint args))

(def botname "hodor")

(defn command? [text]
  (re-matches #"@.*" text))

(defn do-command [irc text nick]
  (case text
    "@quit" (irclj/quit irc)
    (irclj/message irc nick (str "unknown command: " text))
    )
  )

(def throttle-window 20000) ; milliseconds
(def throttle-state (atom {:throttling false
                           :hodors #{}})) ; set of hodor timestamps
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
    {:hodors pruned-hodors
     :throttling new-throttling}))

(defn new-hodor! [{:keys [throttling hodors]}]
  {:throttling throttling
   :hodors (conj hodors (System/currentTimeMillis))})

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

(defn privmsg [irc {:keys [nick text target] :as data}]

  (if (= botname target)
    (if (and (command? text) (= nick "korny"))
      (do-command irc text nick)
      (send-hodor irc nick))
    (if (has-hodor? text)
      (send-hodor irc target)
      #_(prn "no hodor in " text))))

(def callbacks {:privmsg privmsg
                ; :raw-log raw-log
                })

(defn connection [] (irclj/connect
                      "localhost"
                      6667
                      botname
                      :username "hodor"
                      :realname "Hodor"
                      :callbacks callbacks))

(def irc (atom nil))

(defn connect []
  (reset! irc (connection))
  (irclj/join @irc "#hodortest" ))

(connect)

(comment
  (irclj/quit @irc))


