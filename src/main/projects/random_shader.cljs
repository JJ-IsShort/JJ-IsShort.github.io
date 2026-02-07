(ns projects.random-shader
  (:require
   [config-utils :as utils]
   [graphics]
   [structure-editor]
   [styling]))

(def builtin-primitives
  {:random-range (fn [min max] (+ min (* (rand) (- max min))))
   :variable (fn [name] (str name))})

(def test-grammar
  {:Start [[1.0 `((raw "vec3<f32>(") G (raw ",") G (raw ",") G (raw ")"))]]
   :G [[0.5 `(+ {:primitive :variable :args ["x"]} G)]
       [0.25 `(+ G G)]
       [0.25 `(* G G)]
       [0.25 `(/ G G)]
       [0.25 `(/ 1 G)]
       [0.5 `(abs G)]
       [0.5 `(* {:primitive :variable :args ["x"]} G)]
       [0.5 `(* {:primitive :variable :args ["y"]} G)]
       [0.25 `(sin (/ G 100000))]
       [0.5 `((raw "length(coord-vec2(") Term (raw ",") Term (raw "))"))]
       [0.75 `((raw "(floor(") G (raw "*8)/8)"))]]
   :Term [[0.5 {:primitive :random-range :args [-1 1]}]
          [0.25 `(* (+ {:primitive :variable :args ["x"]} {:primitive :random-range :args [-1 1]}) 10)]
          [0.25 `(* (+ {:primitive :variable :args ["y"]} {:primitive :random-range :args [-1 1]}) 10)]
          [0.25 {:primitive :random-range :args [-5 5]}]
          [0.25 `(sin (/ (+ {:primitive :variable :args ["time"]} {:primitive :random-range :args [-3.1415 3.1415]}) {:primitive :random-range :args [0.25 10]}))]
          [0.5 `((raw "(floor(") {:primitive :variable :args ["x"]} (raw "*8)/8)"))]
          [0.5 `((raw "(floor(") {:primitive :variable :args ["y"]} (raw "*8)/8)"))]
          [0.5 `((raw "(floor(") (+ (sin {:primitive :variable :args ["time/8"]}) 1) (raw "*16)/16*4)"))]]})

(def language-dictionary
  {:+ #(str "(" %1 "+" %2 ")")
   :- #(str "(" %1 "-" %2 ")")
   :* #(str "(" %1 "*" %2 ")")
   :/ #(str "(" %1 "/" %2 ")")
   :raw str
   :sin #(str "sin(" % ")")
   :cos #(str "cos(" % ")")
   :abs #(str "abs(" % ")")})

(defn weighted-choice [weighted-productions]
  (let [total (reduce + (map first weighted-productions))
        r (* (rand) total)]
    (loop [acc 0
           [[weight prod] & rest] weighted-productions]
      (let [new-acc (+ acc weight)]
        (if (< r new-acc)
          prod
          (recur new-acc rest))))))

(defn expand [grammar primitives node max-depth]
  (cond
    (list? node)
    (map #(expand grammar primitives % (dec max-depth)) node)

     ;; Leave it alone if it is a language operator
    (or (and (symbol? node) (contains? language-dictionary (keyword (name node))))
        (number? node)
        (string? node))
    node

     ;; Primitive function call
    (and (map? node) (:primitive node))
    (let [{:keys [primitive args]} node
          prim-fn (get primitives primitive)]
      (if prim-fn
        (apply prim-fn args)
        (throw (ex-info "Unknown primitive" {:primitive primitive}))))

     ;; Depth limit reached - return the :Term production
    (<= max-depth 0)
    (expand grammar primitives (weighted-choice (get grammar :Term)) (dec max-depth))

     ;; Non-terminal symbol - look up in grammar
    (and (symbol? node) (contains? grammar (keyword (name node))))
    (let [productions (get grammar (keyword (name node)))
          chosen (weighted-choice productions)]
      (expand grammar primitives chosen (dec max-depth)))

    (and (sequential? node) (not (list? node)))
    (map #(expand grammar primitives % (dec max-depth)) node)

     ;; Plain terminal (number, string, keyword, etc)
    :else
    node))

(defn generate [grammar primitives start-symbol & {:keys [max-depth] :or {max-depth 10}}]
  (expand grammar primitives start-symbol max-depth))

(defn sexpr->shader [sexpr]
  (cond
    (string? sexpr)
    sexpr

    (number? sexpr)
    (.toFixed sexpr 6)

    (and (or (list? sexpr) (coll? sexpr)) (symbol? (first sexpr)) (contains? language-dictionary (keyword (name (first sexpr)))))
    (let [operand (get language-dictionary (keyword (name (first sexpr))))]
      (apply operand (map sexpr->shader (rest sexpr))))

    (coll? sexpr)
    (apply str (map sexpr->shader sexpr))))

(def config
  {:name "CFG based shader generator"
   :id :random-shader
   :callbacks
   {:render (fn [state store] (utils/panel-container
                               (utils/panel "What is this?"
                                            [[(into [:div {:class [:w-222]}]
                                                    (utils/text->divs
                                                     "I saw the youtuber Tsoding's RandomArt project and wanted to mess around with that myself. There were some things I felt I could do better. And I wanted to put it on my website. So, since I'm in the process of making my personal website then I thought this would be a good opportunity to implement my vision for the RandomArt system."))]])
                               (utils/panel "Basic RandomArt"
                                            [[[:div {:class [:flex :flex-row]}
                                               [:div {:class [:h-100 :w-150 :p-2]}
                                                (structure-editor/editor nil "editor-basic" test-grammar)]
                                               [:div {:replicant/on-mount
                                                      #(graphics/create-shader-canvas "Basic RandomArt Canvas" "vec3(x*y)" {:width 100 :height 100})
                                                      :class [:size-100 :p-2 :pl-2]}
                                                (graphics/shader-canvas-hiccup "Basic RandomArt Canvas" [:size-full "aspect-1/1"])]]
                                              [:div {:class [:flex]}
                                               [:div {:class [:w-full :h-6]}
                                                [:div {:class [:margin-auto :h-full :text-center (styling/color-tag "border" :text) :border-2]
                                                       :on {:click #(let [controls (graphics/get-canvas-controls "Basic RandomArt Canvas")]
                                                                      ((:set-shader! controls)
                                                                       (sexpr->shader (generate test-grammar builtin-primitives `Start {:max-depth 35}))))}}
                                                 "Generate"]]]]])))
    :post-render (fn [state store])}})

