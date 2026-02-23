(ns structure-editor
  (:require
   [styling]))

(set! *warn-on-infer* false)

(defonce editors-state (atom {:currently-selected nil :editor-state {}}))

(defn activate-editor! [id]
  (swap! editors-state assoc :currently-selected id))

(defn deactivate-editor! []
  (swap! editors-state assoc :currently-selected nil))

(defn update-structure! [id update-fn]
  (let [old-structure (get-in @editors-state [:editor-state id :current-data])
        new-structure (update-fn id old-structure)
        edited-callback (get-in @editors-state [:editor-state id :edited-callback])]
    ; (swap! editors-state assoc-in [:editor-state id :current-data] new-structure)
    (when edited-callback
      (edited-callback new-structure))))

(def mappings
  (let [change-mode (fn [mode] (swap! editors-state assoc :editor-mode mode))
        key (fn [^js e] (.-key e))
        ctrl? (fn [^js e] (.-ctrlKey e))
        meta? (fn [^js e] (.-metaKey e))
        current-selected (fn [id] (reduce (fn [node key] (println node key) (get-in node key)) (get-in @editors-state [:editor-state id :current-data]) (get-in @editors-state [:editor-state id :current-editing])))]
    {:nav {:name "Navigation"
           :keys {:-> {:name "Select Left" :func (fn [id] (let [current (current-selected id)]
                                                            (println current)))}}}}))

(defn get-node
  [mappings state key-history]
  (let [state-node (get mappings state)]
    (reduce
     (fn [node key]
       (get-in node [:keys key]))
     state-node
     key-history)))

(defn dispatch
  "Given the current state and key history
   returns either:
     {:status :action  :func f    :name s}  — leaf reached, call f
     {:status :prefix  :name s}             — valid prefix, keep buffering
     {:status :no-match}                    — dead end"
  [mappings state key-history]
  (let [node (get-node mappings state key-history)]
    (cond
      (nil? node) {:status :no-match}
      (:func node) {:status :action :func (:func node) :name (:name node)}
      (:keys node) {:status :prefix :name (:name node)}
      :else {:status :no-match})))

(defn to-keycode [e]
  :->)

(defn seq-get-in [data path]
  (reduce (fn [acc k]
            (if (associative? acc)
              (get acc k)
              (nth acc k)))
          data path))

(defn seq-assoc-in [data [k & rest-path] value]
  (if (associative? data)
    (if rest-path
      (assoc data k (seq-assoc-in (get data k) rest-path value))
      (assoc data k value))
    (let [v (vec data)
          updated (if rest-path
                    (seq-assoc-in (nth v k) rest-path value)
                    value)]
      (seq (assoc v k updated)))))

(defn remove-nth [v index]
  (cond
    (vector? v)
    (vec (concat (subvec v 0 index) (subvec v (inc index))))

    (map? v)
    (dissoc v index)

    (seq? v)
    (seq (concat (take index v) (drop (inc index) v)))

    :else
    v))

(defn abort-edit! [id]
  (swap! editors-state update-in [:editing] assoc
         :editing? false
         :edit-buffer ""
         :insert-index 0)
  ((get-in @editors-state [:editor-state id :edited-callback])))

(defn selected-is-map-key? [id]
  (let [path (get-in @editors-state [:editor-state id :current-editing])
        parent-path (vec (drop-last path))
        parent (seq-get-in (get-in @editors-state [:editor-state id :current-data]) parent-path)]
    (map? parent)))

(defn enter-key-rename-mode! [id]
  (let [path    (get-in @editors-state [:editor-state id :current-editing])
        old-key (last path)
        initial (if (keyword? old-key) (name old-key) (pr-str old-key))]
    (swap! editors-state update-in [:editing] assoc
           :editing? true
           :edit-buffer initial
           :insert-index (count initial)
           :renaming-key? true))
  ((get-in @editors-state [:editor-state id :edited-callback])))

(defn enter-edit-mode! [id]
  (let [path    (get-in @editors-state [:editor-state id :current-editing])
        current (seq-get-in (get-in @editors-state [:editor-state id :current-data]) path)
        initial (cond
                  (keyword? current) (name current)
                  (string? current)  current
                  (symbol? current)  (name current)
                  :else              (pr-str current))]
    (when-not (or (seq? current) (vector? current) (map? current))
      (swap! editors-state update-in [:editing] assoc
             :editing? true
             :edit-buffer initial
             :insert-index (count initial)
             :renaming-key? false)))
  ((get-in @editors-state [:editor-state id :edited-callback])))

(defn parse-edit-buffer [buffer original]
  (cond
    (number? original)    (js/parseFloat buffer)
    (keyword? original)   (keyword buffer)
    (symbol? original)    (symbol buffer)
    (string? original)    buffer
    :else                 buffer))

(defn commit-key-rename! [id]
  (let [path      (get-in @editors-state [:editor-state id :current-editing])
        old-key   (last path)
        parent-path (vec (drop-last path))
        buffer    (get-in @editors-state [:editing :edit-buffer])
        new-key   (keyword buffer)
        old-val   (seq-get-in (get-in @editors-state [:editor-state id :current-data])
                              (conj parent-path old-key))]
    (swap! editors-state update-in [:editor-state id :current-data]
           (fn [data]
             (-> data
                 (seq-assoc-in parent-path
                               (-> (seq-get-in data parent-path)
                                   (dissoc old-key)
                                   (assoc new-key old-val))))))
    (swap! editors-state assoc-in [:editor-state id :current-editing]
           (conj parent-path new-key))
    (swap! editors-state update-in [:editing] assoc
           :editing? false :edit-buffer "" :insert-index 0)
    ((get-in @editors-state [:editor-state id :edited-callback]))))

(defn commit-edit! [id]
  (if (get-in @editors-state [:editing :renaming-key?])
    (commit-key-rename! id)
    (let [path   (get-in @editors-state [:editor-state id :current-editing])
          buffer (get-in @editors-state [:editing :edit-buffer])
          orig   (seq-get-in (get-in @editors-state [:editor-state id :current-data]) path)
          parsed (parse-edit-buffer buffer orig)]
      (do
        (swap! editors-state update-in [:editor-state id]
               (fn [s]
                 (update s :current-data #(seq-assoc-in % path parsed))))
        (swap! editors-state update-in [:editing] assoc
               :editing? false
               :edit-buffer ""
               :insert-index 0))))
  ((get-in @editors-state [:editor-state id :edited-callback])))

(defn enter-append-mode! [id]
  (let [path    (get-in @editors-state [:editor-state id :current-editing])
        current (seq-get-in (get-in @editors-state [:editor-state id :current-data]) path)]
    (swap! editors-state update-in [:appending] assoc
           :appending? true))
  ((get-in @editors-state [:editor-state id :edited-callback])))

(defn str-insert
  [s sub i]
  (str (subs s 0 i) sub (subs s i)))

(defn remove-char-at
  [s idx]
  (str (subs s 0 idx) (subs s (inc idx))))

(defn handle-key-event! [e]
  (when-let [active-id (:currently-selected @editors-state)]
    (let [; mode (if (:editor-mode @editors-state) (:editor-mode @editors-state) :nav)
          new-key (to-keycode e)]
          ; keys (conj (if (:keypress-list @editors-state) (:keypress-list @editors-state) []) new-key)
          ; result (dispatch mappings mode keys)]
      (.preventDefault e)
      (.stopPropagation e)
      ; (println keys))))

      ; Run mapping DSL
      ; (case (:status result)
      ;   :action
      ;   (do (swap! editors-state assoc :keypress-list [])
      ;       ((:func result) active-id))
      ;
      ;   :prefix
      ;   (swap! editors-state update :keypress-list conj new-key)
      ;
      ;   :no-match
      ;   (swap! editors-state assoc :keypress-list [])))))

      (let [key (.-key e)
            ctrl? (.-ctrlKey e)
            meta? (.-metaKey e)
            path (get-in @editors-state [:editor-state active-id :current-editing])
            path-to (vec (drop-last path))
            current-selected (seq-get-in (get-in @editors-state [:editor-state active-id :current-data]) (drop-last path))
            selected-index (last path)
            next-key (fn [m current-key]
                       (let [sorted-keys (sort (keys m))
                             probable-next (->> sorted-keys
                                                (drop-while #(not= % current-key))
                                                rest
                                                first)]
                         (if probable-next probable-next (first sorted-keys))))
            previous-key (fn [m current-key]
                           (let [sorted-keys (sort (keys m))
                                 probable-prev (->> sorted-keys
                                                    (take-while #(not= % current-key))
                                                    last)]
                             (if probable-prev probable-prev (last sorted-keys))))]

        (if (get-in @editors-state [:appending :appending?])
          (let [append-index (last path)
                append (fn [obj]
                         (swap! editors-state seq-assoc-in (into [:editor-state active-id :current-data] path-to)
                                (cond
                                  (vector? current-selected)
                                  (vec (concat (subvec current-selected 0 append-index) [obj] (subvec current-selected append-index)))

                                  (seq? current-selected)
                                  (seq (concat (take append-index current-selected) [obj] (drop append-index current-selected)))

                                  (map? current-selected)
                                  (assoc current-selected :New_Key obj)))
                         (swap! editors-state update-in [:appending] assoc
                                :appending? false)
                         ((get-in @editors-state [:editor-state active-id :edited-callback])))]
            (cond
              ; Appending mode
              (= key "s")
              (append "New String")

              (= key "n")
              (append 0)

              (= key "v")
              (append [])

              (= key "l")
              (append `(Dont_Delete))

              (= key "m")
              (append {})

              (= key "k")
              (append :New_Key)

              (= key "y")
              (append `New_Symbol)

              :else
              (do (swap! editors-state update-in [:appending] assoc
                         :appending? false)
                  ((get-in @editors-state [:editor-state active-id :edited-callback])))))
          (if (get-in @editors-state [:editing :editing?])
            (cond
              ; Editing mode
              (= key "Enter") (commit-edit! active-id)
              (= key "Escape")  (abort-edit! active-id)
              (= key "Backspace")
              (do (swap! editors-state update-in [:editing :edit-buffer]
                         #(remove-char-at % (dec (get-in @editors-state [:editing :insert-index]))))
                  (swap! editors-state update-in [:editing :insert-index] #(max 0 (dec %)))
                  ((get-in @editors-state [:editor-state active-id :edited-callback])))
              (= key "ArrowRight")
              (do (swap! editors-state update-in [:editing :insert-index] #(min (count (get-in @editors-state [:editing :edit-buffer])) (inc %)))
                  ((get-in @editors-state [:editor-state active-id :edited-callback])))
              (= key "ArrowLeft")
              (do (swap! editors-state update-in [:editing :insert-index] #(max 0 (dec %)))
                  ((get-in @editors-state [:editor-state active-id :edited-callback])))
              (= (count key) 1)
              (do (swap! editors-state update-in [:editing :edit-buffer] str-insert key (get-in @editors-state [:editing :insert-index]))
                  (swap! editors-state update-in [:editing :insert-index] #(min (count (get-in @editors-state [:editing :edit-buffer])) (inc %)))
                  ((get-in @editors-state [:editor-state active-id :edited-callback]))))

            (cond
              (= key "Escape")
              (deactivate-editor!)

              (= key "ArrowRight")
              (do
                (cond
                  (or (seq? current-selected) (vector? current-selected))
                  (swap! editors-state assoc-in [:editor-state active-id :current-editing] (conj path-to (mod (inc selected-index) (count current-selected))))

                  (map? current-selected)
                  (swap! editors-state assoc-in [:editor-state active-id :current-editing] (conj path-to (next-key current-selected selected-index)))

                  :else
                  (println current-selected (type current-selected)))
                ((get-in @editors-state [:editor-state active-id :edited-callback])))

              (= key "ArrowLeft")
              (do
                (cond
                  (or (seq? current-selected) (vector? current-selected))
                  (swap! editors-state assoc-in [:editor-state active-id :current-editing] (conj path-to (mod (+ (- selected-index 1) (count current-selected)) (count current-selected))))

                  (map? current-selected)
                  (swap! editors-state assoc-in [:editor-state active-id :current-editing] (conj path-to (previous-key current-selected selected-index)))

                  :else
                  (println current-selected (type current-selected)))
                ((get-in @editors-state [:editor-state active-id :edited-callback])))

              (= key "ArrowDown")
              (do (let [target-expr (if (associative? current-selected) (get current-selected selected-index) (nth current-selected selected-index))]
                    (cond
                      (or (seq? target-expr) (vector? target-expr))
                      (swap! editors-state assoc-in [:editor-state active-id :current-editing] (conj path 0))

                      (map? target-expr)
                      (swap! editors-state assoc-in [:editor-state active-id :current-editing] (conj path (first (sort (keys target-expr)))))

                      :else
                      (println target-expr (type target-expr))))
                  ((get-in @editors-state [:editor-state active-id :edited-callback])))

              (= key "ArrowUp")
              (when (> (count (drop-last path)) 0)
                (do (swap! editors-state assoc-in [:editor-state active-id :current-editing] (vec (drop-last path)))
                    ((get-in @editors-state [:editor-state active-id :edited-callback]))))

              (and (= key "d") ctrl?)
              (update-structure!
               active-id
               (fn [id structure]
                 (swap! editors-state seq-assoc-in (into [:editor-state active-id :current-data] (drop-last path))
                        (remove-nth current-selected selected-index))
                 (swap! editors-state assoc-in [:editor-state active-id :current-editing]
                        (if (> (count (drop-last path)) 0) (vec (drop-last path))
                            [(cond
                               (map? current-selected)
                               (first (sort (keys current-selected)))

                               (or (seq? current-selected) (vector? current-selected))
                               0)]))))
                 ; ((get-in @editors-state [:editor-state active-id :edited-callback]))))

              (and (= key "e") ctrl?)
              (enter-edit-mode! active-id)

              (and (= key "r") ctrl?)
              (when (selected-is-map-key? active-id)
                (enter-key-rename-mode! active-id))

              (and (= key "a") ctrl?)
              (enter-append-mode! active-id))))))))

        ; (cond
        ;   (= mode "nav")
        ;   (cond
        ;      ;; Escape to deactivate
        ;     (= key "Escape")
        ;     (deactivate-editor!)
        ;
        ;      ;; Next adjacent elem
        ;     (= key "ArrowRight")
        ;     ())
        ;
        ;   (= mode "edit")
        ;   (cond
        ;      ;; Delete current element
        ;     (= key "Backspace")
        ;     (update-structure! active-id
        ;                        (fn [structure]
        ;                          structure))
        ;
        ;      ;; Add new element
        ;     (and (or ctrl? meta?) (= key "n"))
        ;     (update-structure! active-id
        ;                        (fn [structure]
        ;                          (conj structure :new-element)))
        ;
        ;      ;; Character input
        ;     :else
        ;     (when (= 1 (count key))
        ;       (update-structure! active-id
        ;                          (fn [structure]
        ;                            structure)))))))))

(defn init-global-handlers! []
  (when-not (:initialized? @editors-state)
    (.addEventListener js/document "keydown" handle-key-event!)
    (swap! editors-state assoc :initialized? true)))

(defn get-structure [id]
  (get-in @editors-state [:editor-state (keyword id) :current-data]))

(defn render-structure [structure cursor-path visual-col selected-path]
  (let [colors {:non-text "text-[color-mix(in_hsl,var(--color-highlight)_100%,_transparent_0%)]"
                :symbol :text-blue-400
                :vector-decorator :text-purple-400
                :string :text-green-300
                :keyword :text-red-400
                :map-keys :text-pink-300
                :number "text-[#f1f1f1]"
                :selected "bg-[color-mix(in_hsl,#f1f1f1_40%,_transparent_60%)]"}
        current-indent (apply str (repeat visual-col " "))]
    (cond
      (vector? structure)
      (let [vec-size (count structure)]
        [:span {:class []}
         "["
         (for [[i item] (map-indexed vector structure)]
           ^{:key i}
           (list
            (when (> vec-size 2)
              [:br])
            (when (> vec-size 2)
              [:span current-indent " "])
            ; (when (seq structure) " ")

            [:span {:class [(when (= (conj cursor-path i) selected-path) (:selected colors)) (:vector-decorator colors)]
                    :data-selected (when (= (conj cursor-path i) selected-path) "true")}
             (render-structure item (conj cursor-path i) (inc visual-col) selected-path)]
            (when (< i (dec (count structure))) " ")))
         (when (> vec-size 2)
           [:br])
         (when (> vec-size 2)
           [:span current-indent])
         "]"])

      (map? structure)
      (let [map-size (count structure)]
        [:span {:class [(:map-keys colors)]}
         "{"
         (for [[k v] (sort structure)]
           ^{:key k}
           [:span {:data-selected (when (= (conj cursor-path k) selected-path) "true")}
            (when (> map-size 2)
              [:br])
            (when (> map-size 2)
              [:span current-indent " "])
            [:span {:class [(:map-keys colors)]}
             ":"
             [:span {:class [(when (= (conj cursor-path k) selected-path) (:selected colors))]} (name k)]
             " "]
            [:span {:class [(styling/color-tag "text" :text)]} (render-structure v (conj cursor-path k) (inc visual-col) selected-path)]
            " "])
         (when (> map-size 2)
           [:br])
         (when (> map-size 2)
           [:span current-indent])
         "}"])

      (seq? structure)
      [:span {:class [(:non-text colors)]}
       "("
       (for [[i item] (map-indexed vector structure)]
         ^{:key i}
         (list [:span {:class [(when (= (conj cursor-path i) selected-path) (:selected colors))]
                       :data-selected (when (= (conj cursor-path i) selected-path) "true")}
                (render-structure item (conj cursor-path i) (inc visual-col) selected-path)]
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

      (symbol? structure)
      [:span {:class [(:symbol colors)]} (name structure)]

      :else
      [:span {:class [:text-red-400]} (pr-str structure) " :: " (type->str (type structure))])))

(defn scroll-selected-into-view! [editor-id]
  (when-let [editor-el (.getElementById js/document (name editor-id))]
    (when-let [selected-el (.querySelector editor-el "[data-selected='true']")]
      (.scrollIntoView selected-el #js {:block "nearest" :inline "nearest"}))))

(defn editor [edited-callback id default-structure]
  ; Set up the state in the atom
  (when-not (get-in @editors-state [:editor-state (keyword id)])
    (swap! editors-state assoc-in [:editor-state (keyword id)] {:element nil
                                                                :current-data default-structure
                                                                :edited-callback edited-callback
                                                                :current-editing [(let [fst (first default-structure)]
                                                                                    (cond
                                                                                      (seq? fst)
                                                                                      fst

                                                                                      (map-entry? fst)
                                                                                      (key fst)

                                                                                      :else
                                                                                      fst))]}))
  ; Define the update and render function and bind them
  [:div {:class ["bg-[#121212]" :border-dashed
                 :border-2 "border-[#f1f1f1]" :size-full :overflow-hidden :p-2]
         :id id
         :on {:click #(swap! editors-state assoc :currently-selected (keyword id))}
         :replicant/on-mount #(init-global-handlers!)
         :replicant/on-render #(scroll-selected-into-view! (keyword id))}
   [:pre {:class [:overflow-hidden :w-full "h-[calc(100%-9*var(--spacing))]"]}
    (render-structure (get-in @editors-state [:editor-state (keyword id) :current-data]); [:test {:test [:other "test" {:hey `lets :make "this" :way `(more [complicated okay ?])}]}]
                      [] 0 (get-in @editors-state [:editor-state (keyword id) :current-editing]))]
   [:div {:class [:w-full :h-8 :border-1 :border-dashed "border-[#f1f1f1]" "bg-[#121212]" :mt-1 :flex :flex-row]}
    (if (get-in @editors-state [:appending :appending?])
      [:div {:class [:h-full "w-2" :bg-red-300]}]
      (if (get-in @editors-state [:editing :editing?])
        (list
         [:div {:class [:h-full "w-2" :bg-green-400]}]
         [:div {:class ["w-[calc(100%_-_var(--spacing)*(2+20))]"]}]
         [:div {:class [:text-center :flex :h-full :items-center :mr-2]}
          [:div {:class [:h-fit "text-[#f1f1f1]"]} (get-in @editors-state [:editing :edit-buffer])]])
        (list
         [:div {:class [:h-full "w-2" :bg-blue-300]}])))]])
