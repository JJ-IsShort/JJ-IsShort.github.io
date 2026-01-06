(ns routing
  (:require
   [clojure.string :as s]
   [config :as c]))

(defn extract-location [path]
  (or (when (or (= "/" path)
                (= "" path))
        {:location/page-id (nth c/page-names 0)})
      (when-let [[_ target] (re-find #"/?#/(\w+)/?" path)]
        (when (some #(= (s/replace target #"_" " ") %) c/page-names)
          {:location/page-id (s/replace target #"_" " ")
           :location/path path}))))
