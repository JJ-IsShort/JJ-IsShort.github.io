(ns structure-editor
  (:require
   [styling]))

(defonce editors-state (atom {:currently-selected nil :editor-state {}}))

(defn activate-editor! [id]
  (swap! editors-state assoc :currently-selected id))

(defn deactivate-editor! []
  (swap! editors-state assoc :currently-selected nil))

(defn update-structure! [id update-fn]
  (let [old-structure (get-in @editors-state [:editors id :current-data])
        new-structure (update-fn old-structure)
        edited-callback (get-in @editors-state [:editors id :edited-callback])]
    (swap! editors-state assoc-in [:editors id :structure] new-structure)
    (when edited-callback
      (edited-callback new-structure))))

(defn get-structure [id]
  (get-in @editors-state [:editor-state id :structure]))

(defn render-structure [structure cursor-path visual-col]
  (let [colors {:non-text "text-[color-mix(in_hsl,var(--color-highlight)_100%,_transparent_0%)]"
                :symbol :text-blue-400
                :vector-decorator :text-purple-400
                :string :text-green-300
                :keyword :text-red-400
                :map-keys :text-pink-300
                :number "text-[#f1f1f1]"}
        current-indent (apply str (repeat visual-col " "))]
    (cond
      (vector? structure)
      [:span {:class [(:vector-decorator colors)]}
       "["
       (for [[i item] (map-indexed vector structure)]
         ^{:key i}
         [:span
          [:br]
          [:span current-indent " "]
          ; (when (seq structure) " ")
          (render-structure item (conj cursor-path i) (inc visual-col))
          (when (< i (dec (count structure))) " ")])
       [:br]
       [:span current-indent]
       "]"]

      (map? structure)
      [:span {:class [(:map-keys colors)]}
       "{"
       (for [[k v] structure]
         ^{:key k}
         [:span
          [:br]
          [:span current-indent " "]
          [:span {:class [(:map-keys colors)]}
           ":"
           [:span (name k)]
           " "
           [:span {:class [(styling/color-tag "text" :text)]} (render-structure v (conj cursor-path k) (inc visual-col))]]])
       [:br]
       [:span current-indent]
       "}"]

      (list? structure)
      [:span {:class [(:non-text colors)]}
       "("
       (for [[i item] (map-indexed vector structure)]
         ^{:key i}
         (list (render-structure item (conj cursor-path i) (inc visual-col))
               (when (< i (dec (count structure))) " ")))
       ")"]

      (keyword? structure)
      [:span {:class [(:keyword colors)]}
       ":" [:span (name structure)]]

      (string? structure)
      [:span {:class [(:string colors)]}
       "\"" [:span structure] "\""]

      (number? structure)
      [:span {:class [(:number colors)]} (pr-str structure)]

      :else
      [:span {:class [(:symbol colors)]} (name structure)])))

(defn editor [edited-callback id default-structure]
  ; Set up the state in the atom
  (swap! editors-state assoc-in [:editor-state (keyword id)] {:element nil
                                                              :current-data default-structure
                                                              :edited-callback edited-callback
                                                              :current-editing [0]})
  ; Define the update and render function and bind them
  [:div {:class ["bg-[#121212]" :border-dashed
                 :border-2 "border-[#f1f1f1]" :size-full :overflow-hidden :p-2]}
   [:pre {:class [:overflow-auto :size-full]}
    (render-structure default-structure; [:test {:test [:other "test" {:hey `lets :make "this" :way `(more [complicated okay ?])}]}]
                      [] 0)]])

(defn handle-key-event! [e]
  (when-let [active-id (:currently-selected @editors-state)]
    (.preventDefault e)
    (.stopPropagation e)

    (let [key (.-key e)
          ctrl? (.-ctrlKey e)
          meta? (.-metaKey e)]

      (cond
        ;; Escape to deactivate
        (= key "Escape")
        (deactivate-editor!)

        ;; Delete current element
        (= key "Backspace")
        (update-structure! active-id
                           (fn [structure]
                             structure))

        ;; Add new element
        (and (or ctrl? meta?) (= key "n"))
        (update-structure! active-id
                           (fn [structure]
                             (conj structure :new-element)))

        ;; Character input
        :else
        (when (= 1 (count key))
          (update-structure! active-id
                             (fn [structure]
                               structure)))))))

(defn init-global-handlers! []
  (when-not (:initialized? @editors-state)
    (.addEventListener js/document "keydown" handle-key-event!)
    (swap! editors-state assoc :initialized? true)))
