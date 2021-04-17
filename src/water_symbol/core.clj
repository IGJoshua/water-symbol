(ns water-symbol.core
  (:require
   [farolero.core :as far]
   [s-expresso.ecs :as ecs]
   [s-expresso.engine :as se]
   [s-expresso.render :as r]
   [s-expresso.window :as wnd]
   [s-expresso.resource :refer [with-free]])
  (:gen-class))

(defonce input (atom {}))

(defn set-close
  [window]
  (swap! input assoc :should-close? true)
  (wnd/window-should-close window false))

(def window-opts
  {:request-close-callback #'set-close
   :title "Water Symbol"})

(defmulti render-event-handler
  (fn [render-state event]
    (::type event)))

(defmethod render-event-handler :default
  [render-state event]
  (far/restart-case (far/warn ::unrecognized-event)
    (::far/continue []
      :report "Retry handling the event"
      :interactive (constantly nil)
      (render-event-handler render-state event))
    (::far/abort []
      :report "Abort handling the event and use the old state"
      :interactive (constantly nil)
      render-state)))

(defn resolve-input
  [state dt]
  (if (:should-close? @input)
    (do (swap! input dissoc :should-close?)
        (assoc state ::se/should-close? true))
    state))

(def init-game-state
  {::ecs/systems [#'resolve-input]
   ::ecs/entities {}
   ::ecs/events []
   ::se/event-handler #'render-event-handler
   ::r/systems []})

(def init-render-state
  {::r/resolvers {}
   ::r/resources {}})

(defn run
  []
  (with-free [window (wnd/make-window window-opts)]
    (wnd/show-window window)
    (se/start-engine window
                     init-game-state
                     init-render-state
                     (/ 60))))

(defn -main
  [& args]
  (wnd/init-glfw)
  (run)
  (wnd/shutdown-glfw))
