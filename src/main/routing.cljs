(ns routing
  (:require
   [clojure.string :as s]))
   ; [config :refer [page-names]]))

(defn extract-location [path page-names]
  (or (when (or (= "/" path)
                (= "" path))
        {:location/page-id (nth page-names 0)})
      (when-let [[_ target args] (re-find #"/?#/(\w+)/?(.*)?" path)]
        (let [proper-name (s/replace target #"_" " ")]
          (if (some #(= proper-name %) page-names)
            {:location/page-id proper-name
             :location/path args}
            {:location/page-id (nth page-names 0)})))))
