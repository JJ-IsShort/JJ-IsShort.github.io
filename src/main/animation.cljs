(ns animation)

(defonce animated-values (atom {}))
(defonce animation-running (atom false))

(defn start-animation-loop []
  (when-not @animation-running
    (reset! animation-running true)
    (letfn [(animate []
              (doseq [[id {:keys [current target rate apply-fn element-id set-fn]}] @animated-values]
                (let [diff (- target current)]
                  (when (> (js/Math.abs diff) 0.01)
                    (let [new-current (+ (* (- current target) rate) target)
                          value-str (apply-fn new-current)]
                      (swap! animated-values assoc-in [id :current] new-current)
                      (when-let [element (.getElementById js/document element-id)]
                        (set-fn element value-str))))))
              (js/requestAnimationFrame animate))]
      (animate))))

(defn animatable
  ([id initial-value target-value rate apply-fn element-id set-fn]
   (when-not (get @animated-values id)
     (swap! animated-values assoc id {:current initial-value
                                      :target initial-value
                                      :rate rate
                                      :apply-fn apply-fn
                                      :element-id element-id
                                      :set-fn set-fn})
     (start-animation-loop))

   (swap! animated-values assoc-in [id :target] target-value)

   ;; Return the current value as a string so that the Hiccup has the right value
   (apply-fn (get-in @animated-values [id :current] initial-value))))

(defn get-current
  ([id]
   (get-in @animated-values [id :current])))

