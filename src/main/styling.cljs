(ns styling)

(defn color [colors type]
  (let [selector (if (keyword? type) type (if (string? type) (keyword type) :highlight))]
    (get colors selector)))

(def color-schemes
  [{:base "#121212"
    :text "#f1f1f1"
    :highlight "#8540c9"}
   {:base "#fafafa"
    :text "#020202"
    :highlight "#020202"}])

(defn set_colors [index]
  (let [computed-style (.-style (.querySelector js/document ":root"))
        default-colors (get color-schemes index)]
    (.setProperty computed-style "--color-base" (color default-colors :base))
    (.setProperty computed-style "--color-text" (color default-colors :text))
    (.setProperty computed-style "--color-highlight" (color default-colors :highlight))))

(defn color-tag [prefix type & [suffix]]
  (str prefix "-(--color-" (name type) ")" suffix))

