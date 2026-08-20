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
    :highlight "#020202"}
   {:base "#24292E"
    :text "#A9B9CE"
    :highlight "#A35C50"}
   {:base "#1F1F1F" ; Colour schemes past this point inspired by the Intent Editor
    :text "#E9E5E5"
    :highlight "#FFE4C4"}
   {:base "#FEF8EB"
    :text "#222222"
    :highlight "#B4B4B4"}
   {:base "#282C34"
    :text "#E9E5E5"
    :highlight "#4F4F4F"}
   {:base "#F7EEE8"
    :text "#000000"
    :highlight "#EC76AB"}])

(defn set_colors [index]
  (let [computed-style (.-style (.querySelector js/document ":root"))
        default-colors (get color-schemes index)]
    (.setProperty computed-style "--color-base" (color default-colors :base))
    (.setProperty computed-style "--color-text" (color default-colors :text))
    (.setProperty computed-style "--color-highlight" (color default-colors :highlight))))

(defn color-tag [prefix type & [suffix]]
  (str prefix "-(--color-" (name type) ")" suffix))

