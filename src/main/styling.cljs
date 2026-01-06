(ns styling)

(defn color [colors type]
  (let [selector (if (keyword? type) type (if (string? type) (keyword type) :highlight))]
    (get colors selector)))

(defn color-tag [prefix type & [suffix]]
  (str prefix "-(--color-" (name type) ")" suffix))

