(ns main
  (:require [replicant.dom :as r]
            [clojure.string :as s])
  (:import [goog.net Cookies])
  (:require [animation]
            [styling])
  (:require [routing])
  (:require [config]))

(def mouse-pos (atom {:x 0 :y 0}))
(defonce el (js/document.getElementById "app"))
(defonce store (atom {}))
(def cookies (Cookies. js/document))

(defn page-name-rotation [mouse-y-start mouse-y-current i hidden]
  (if hidden
    (let* [y-diff (- mouse-y-current mouse-y-start)
           y-diff-scaled (/ y-diff 150)]
      (+ 0 (* (- 60 0) y-diff-scaled) (* -20 i))) -90))

(defn handle-next-click [callback]
  (js/setTimeout
   (fn []
     (letfn [(handler [e]
               (callback e)
               (.removeEventListener js/document "click" handler))]
       (.addEventListener js/document "click" handler)))
   0))

(.addEventListener js/window "popstate" 
                   (fn []
                    (swap! store assoc :selected-page (routing/extract-location
                                                        js/location.hash config/page-names))))

(defn handle-page-select-interact
  ([e] (let [angle (mod (animation/get-current :pageNamePanel-First-Rot) 360)]
         (when-not
          (or (> angle (+ 8 (* 20 (count (:page-names @store)))))
              (< angle 12)) (let [page-index (int (/ (- angle 10) 20))
                                  page-name (s/replace (nth (:page-names @store) page-index) #" " "_")]
                              (.preventDefault e)
                              (.pushState js/history "" "" (str "#/" page-name "/"))
                              (swap! store assoc :selected-page (routing/extract-location
                                                                 (str "#/" page-name "/")
                                                                 config/page-names))))
         (swap! store assoc-in [:taskbar-page-select :active] false))))

; yoinked from https://github.com/reagent-project/reagent-utils/blob/master/src/reagent/cookies.cljs
(defn set-cookie!
  "sets a cookie, the max-age for session cookie
   following optional parameters may be passed in as a map:
   :max-age - defaults to -1
   :path - path of the cookie, defaults to the full request path
   :domain - domain of the cookie, when null the browser will use the full request host name
   :raw? - format or store raw string
   :secure - to store securely
  "
  [k content & [{:keys [max-age path domain raw? secure] :or {max-age -1 path "/" domain nil raw? true secure true} :as opts}]]
  (let [k (name k)
        content (if raw?
                  (str content)
                  (pr-str content))]
    (cond
      (empty? (dissoc opts :raw?))
      (.set cookies k content)

      :else
      (.set cookies k content max-age path domain secure))))

(defn get-cookie [name default-val]
  (if-let [val (.get cookies name)]
    val
    default-val))

(defn settings_bar [state]
  (let [opened (get-in state [:settings-opened] false)]
    [:div {:class [:transition-transform
                   :mt-20 :pointer-events-auto
                   :w-80 :z-50
                   (when (not opened) :-translate-x-80)
                   :h-full]}
     [:div {:class ["w-[calc(100%-3*var(--spacing)*2)]" :h-full :m-3 :flex :justify-center]}
      [:div {:class [:h-fit-content :w-full]}
       [:div {:class [:relative]}
        [:div {:class [:w-full :h-fit
                       (styling/color-tag "border" :highlight) :border-3
                       (styling/color-tag "bg" :base) :px-3 :py-1 :my-1
                       :rounded-tr-lg :rounded-bl-lg :text-sm]
               :style {:corner-shape "notch"}}
         [:div {:class [:grid :grid-cols-3 :gap-4]}
          (for [[id color] (map-indexed vector styling/color-schemes)]
            [:div {:class [:h-6 :flex :flex-row]
                   :on {:click #(do (styling/set_colors id)
                                    (set-cookie! "selected-color" id))}}
             [:div {:class [:h-full (str "bg-[" (styling/color color :base) "]") :flex-1
                            :rounded-bl-lg :rounded-tl-lg]}]
             [:div {:class [:h-full (str "bg-[" (styling/color color :text) "]") :flex-1]}]
             [:div {:class [:h-full (str "bg-[" (styling/color color :highlight) "]") :flex-1
                            :rounded-br-lg :rounded-tr-lg]}]])]]]]]]))

(defn make-page [state]
  [:div {:class [:min-h-screen]}
   (when (get state :show_header true)
     [:div {:class [:fixed :inset-0 :overflow-hidden :pointer-events-none :z-50]}
      [:div {:class ["w-[calc(100%_-_var(--spacing)_*_4)]" :border-3 (styling/color-tag "border" :highlight)
                     :h-16 :m-2 (styling/color-tag "bg" :base) :absolute :flex "z-50" :pointer-events-auto]}
       [:div {:class ["basis-1/3"]} ; Left bar section
        [:div {:class ["my-[5px]" "h-[calc(100%-5px*2)]" "ml-[5px]" :rounded-md :relative]}
         [:span {:class ["material-symbols-outlined"
                         :h-full
                         :aspect-square
                         :block
                         :cursor-pointer]
                 :style {:color "var(--color-text)"
                         :font-size "300%"
                         :line-height 1}
                 :on {:click #(swap! store assoc :settings-opened (not (get-in state [:settings-opened] false)))}} "settings"]]]
       [:div {:class ["basis-1/3"]}] ; Centre bar section
       [:div {:class ["basis-1/3"]} ; Right bar section
        [:div {:class ["my-[5px]" "h-[calc(100%-5px*2)]" "mr-[5px]" :rounded-md :relative :cursor-pointer]
               :id "pageNameParent"
               :on (when-not (:active (:taskbar-page-select state))
                     {:click #_{:clj-kondo/ignore [:unused-binding]}
                      (fn [e] (do (swap! store assoc :taskbar-page-select {:active true :mouse-y-start (:y mouse-pos)})
                                  (handle-next-click handle-page-select-interact)))})}
         [:div {:class ["bg-[image:radial-gradient(_var(--color-base)_40%,_transparent_60%)]"
                        :size-256 :absolute "top-[50%]" "left-[100%]" "translate-[-50%]"
                        :transition-transform
                        (if (:active (:taskbar-page-select state)) :scale-100 :scale-0) :duration-700]
                :id "pageNameObscure"}]

         [:div {:class [:absolute "size-[48px]" "-left-[40px]"]}
          [:img {:id "pageNameSelector"
                 :style {:translate (animation/animatable :pageNameSelector-Trans -80
                                                          (if (:active (:taskbar-page-select state)) 0 -80)
                                                          0.9 #(str "0px " % "px") "pageNameSelector"
                                                          (fn [el val] (set! (.-translate (.-style el)) val)))
                         :mask "url(\"assets/app/svg/page_name_selector.svg\") no-repeat center"
                         :-webkit-mask "url(\"assets/app/svg/page_name_selector.svg\") no-repeat center"
                         :width 40 :height 40
                         :background-color "var(--color-text)"}}]]
         [:div {:class [:absolute "size-[48px]" "-right-[48px]"]}
          [:div {:class [:relative]}
           [:img {:class [:scale-300 :absolute]
                  :id "pageNamePanel-Dial"
                  :style {:rotate (animation/animatable :pageNamePanel-Dial-Rot 0
                                                        (if (:active (:taskbar-page-select state))
                                                          (page-name-rotation (:mouse-y-start (:taskbar-page-select state)) (:y mouse-pos) 0 (:active (:taskbar-page-select state)))
                                                          0)
                                                        0.9 #(str % "deg") "pageNamePanel-Dial"
                                                        (fn [el val] (set! (.-rotate (.-style el)) val)))
                          :translate (animation/animatable :pageNamePanel-Dial-Trans 60
                                                           (if (:active (:taskbar-page-select state)) 0 60)
                                                           0.9 #(str % "px") "pageNamePanel-Dial"
                                                           (fn [el val] (set! (.-translate (.-style el)) val)))
                          :mask "url(\"assets/app/svg/page_name_dial.svg\") no-repeat center"
                          :-webkit-mask "url(\"assets/app/svg/page_name_dial.svg\") no-repeat center"
                          :width 48 :height 48
                          :background-color "var(--color-text)"}}]]]

         [:div {:class [:absolute "top-1/2" "right-0" "-translate-y-1/2" :size-full]
                :id "pageNameRotate"}
          [:div {:class ["h-[48px]" :flex :items-center :pr-2 :absolute "right-0"
                         "origin-[calc(100%+80px)_50%]"]
                 :id "pageNamePanel-First"
                 :style {:rotate (animation/animatable :pageNamePanel-First-Rot 0
                                                       (if (:active (:taskbar-page-select state))
                                                         (page-name-rotation (:mouse-y-start (:taskbar-page-select state)) (:y mouse-pos) 0 (:active (:taskbar-page-select state)))
                                                         0)
                                                       0.9 #(str % "deg") "pageNamePanel-First"
                                                       (fn [el val] (set! (.-rotate (.-style el)) val)))
                         :translate (animation/animatable :pageNamePanel-First-Trans 0
                                                          (if (:active (:taskbar-page-select state)) -60 0)
                                                          0.9 #(str % "px") "pageNamePanel-First"
                                                          (fn [el val] (set! (.-translate (.-style el)) val)))}}
           [:div {:class [:ml-2 :h-fit]}
            [:div {:class [:text-2xl :font-black]}
             (:location/page-id (:selected-page state))]]]
          (for [[i page-name] (map-indexed vector (:page-names state))]
            (let [id (str "pageNamePanel-" (hash i))]
              [:div {:class ["h-[48px]" :flex :items-center :pr-2 :absolute "right-0"
                             "origin-[calc(100%+80px)_50%]"]

                     :id id
                     :style {:rotate (animation/animatable (keyword (str id "-Rot")) -90
                                                           (page-name-rotation (:mouse-y-start (:taskbar-page-select state)) (:y mouse-pos) (inc i) (:active (:taskbar-page-select state)))
                                                           0.9 #(str % "deg") id
                                                           (fn [el val] (set! (.-rotate (.-style el)) val)))
                             :scale (animation/animatable (keyword (str id "-Scale")) -90
                                                          (if (< (abs (animation/get-current (keyword (str id "-Rot")))) 8) 1.2 1)
                                                          0.8 #(str %) id
                                                          (fn [el val] (set! (.-scale (.-style el)) val)))
                             :translate (animation/animatable (keyword (str id "-Trans")) 0
                                                              (if (:active (:taskbar-page-select state)) -60 0)
                                                              0.9 #(str % "px") id
                                                              (fn [el val] (set! (.-translate (.-style el)) val)))}}
               [:div {:class [:ml-2 :w-full :h-fit]}
                [:div {:class [:text-lg :font-black]}
                 page-name]]]))]]]]
      (settings_bar state)])
   [:div {:class [:w-full :min-h-screen :pointer-events-none]}
    [:div {:class [:w-full :h-full (styling/color-tag "bg" :base) "-z-50" :fixed]}]
    (when (get state :show_header true)
      [:div {:class [:w-full :h-20 :pointer-events-none]}])
    (when (get state :show_header true)
      [:div {:class [:w-full :h-8 :pointer-events-none]}])
    [:div {:class [:pointer-events-auto]}
     ((config/get-page-def (:location/page-id (:selected-page state)) :render) state store)]
    [:div {:class [:w-full :h-8]}]]])

(defn mouse-update [state]
  (animation/animatable :pageNamePanel-Dial-Rot 0
                        (if (:active (:taskbar-page-select @store))
                          (page-name-rotation (:mouse-y-start (:taskbar-page-select @store)) (:y state) 0 (:active (:taskbar-page-select @store)))
                          0)
                        0.9 #(str % "deg") "pageNamePanel-Dial"
                        (fn [el val] (set! (.-rotate (.-style el)) val)))
  (animation/animatable :pageNamePanel-First-Rot 0
                        (if (:active (:taskbar-page-select @store))
                          (page-name-rotation (:mouse-y-start (:taskbar-page-select @store)) (:y state) 0 (:active (:taskbar-page-select @store)))
                          0)
                        0.9 #(str % "deg") "pageNamePanel-First"
                        (fn [el val] (set! (.-rotate (.-style el)) val)))
  (doseq [[i page-name] (map-indexed vector (:page-names @store))]
    (let [id (str "pageNamePanel-" (hash i))]
      (animation/animatable (keyword (str id "-Rot")) -90
                            (page-name-rotation (:mouse-y-start (:taskbar-page-select @store)) (:y state) (inc i) (:active (:taskbar-page-select @store)))
                            0.9 #(str % "deg") id
                            (fn [el val] (set! (.-rotate (.-style el)) val)))
      (animation/animatable (keyword (str id "-Scale")) -90
                            (if (< (abs (animation/get-current (keyword (str id "-Rot")))) 8) 1.2 1)
                            0.8 #(str %) id
                            (fn [el val] (set! (.-scale (.-style el)) val))))))

(defn ^:dev/after-load start []
  (add-watch
   store ::render
   (fn [_ _ _ state]
     (r/render el (make-page state))
     (when-let [post-render-func (config/get-page-def (:location/page-id (:selected-page state)) :post-render)]
       (post-render-func state store))))
  (add-watch
   mouse-pos ::render
   (fn [_ _ _ state]
     (mouse-update state)))

  (styling/set_colors (js/parseInt (get-cookie "selected-color" 0)))

  ;; Trigger the initial render
  (reset! store {:page-names config/page-names
                 :taskbar-page-select {:active false :mouse-y-start 0}
                 :selected-page (routing/extract-location js/location.hash config/page-names)})
  (reset! mouse-pos {:x 0 :y 0}))

; (js/window.addEventListener
;      "popstate"
;      (fn [_]
;        (nxr/dispatch system nil
;         [[:store/assoc-in [:location] (get-current-location)]])))

(start)

(js/addEventListener "mousemove" (fn [e] ; (set! mouse-pos {:x (.-x e) :y (.-y e)})
                                   (reset! mouse-pos {:x (.-x e) :y (.-y e)})) el)
