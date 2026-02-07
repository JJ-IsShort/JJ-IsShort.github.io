(ns config-utils
  (:require
   [clojure.string :as s]
   [styling]))

(defn inspect [object] #_{:clj-kondo/ignore [:redundant-do]}
  (do (println object) object))
(def ins inspect)

(defn panel-container [& panels]
  [:div {:class [:flex :justify-center]}
   [:div {:class [:flex :flex-col
                  :gap-4 :w-fit]}
    panels]])

(defn panel [name content]
  [:div {:class [:h-fit :w-full :flex :justify-center]}
   [:div {:class [:min-w-222 :h-fit :w-fit]}
    [:div {:class [:relative]}
     [:div {:class [:w-full :h-full :absolute :top-1 :right-1
                    "bg-[color-mix(in_hsl,var(--color-highlight)_50%,_transparent_50%)]"
                    "-z-10" :rounded-tr-lg :rounded-bl-lg]
            :style {:corner-shape "notch"}}]
     [:div {:class [:w-full :h-fit (styling/color-tag "border" :highlight)
                    (styling/color-tag "bg" :base) :border-3 :mb-1
                    :rounded-tr-lg :rounded-bl-lg]
            :style {:corner-shape "notch"}}
      [:div {:class [:mx-3 :my-1 :underline :font-bold]}
       name]]]
    (for [panel-contents content]
      [:div {:class [:relative]}
       [:div {:class [:w-full :h-full :absolute :top-1 :right-1
                      "bg-[color-mix(in_hsl,var(--color-highlight)_50%,_transparent_50%)]"
                      "-z-10" :rounded-tr-lg :rounded-bl-lg]
              :style {:corner-shape "notch"}}]
       (into [:div {:class [:w-full :h-fit (styling/color-tag "border" :highlight) :border-3
                            (styling/color-tag "bg" :base) :px-3 :py-1 :my-1
                            :rounded-tr-lg :rounded-bl-lg :text-sm]
                    :style {:corner-shape "notch"}}]
             panel-contents)])]])

(defn text->divs [text & {:keys [class]}]
  (->> (s/split-lines text)
       (map s/trim)
       (mapv (fn [line]
               (if-not (= line "")
                 [:p (when class {:class class})
                  line]
                 [:br (when class {:class class})
                  " "])))))
