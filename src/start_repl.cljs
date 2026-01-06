(ns user
  (:require [shadow.cljs.devtools.api :as shadow]))

(defn cljs-repl
  "Connects to a given build-id. Defaults to `:app`."
  ([]
   (cljs-repl :app))
  ([build-id]
   (shadow/watch build-id)
   (shadow/nrepl-select build-id)))
(cljs-repl :app)
