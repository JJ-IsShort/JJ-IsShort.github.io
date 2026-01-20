(ns config
  (:require
   [clojure.string :as s]
   [styling]
   [graphics]))

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
  (->> (clojure.string/split-lines text)
       (map clojure.string/trim)
       (mapv (fn [line]
               (if-not (= line "")
                 [:p (when class {:class class})
                  line]
                 [:br (when class {:class class})
                  " "])))))

(def site-definition
  [{:Main_Page (fn [state] (panel-container
                            (panel "About Me"
                                   [[[:div {:class [:flex :flex-row]}
                                      [:div {:class [:size-100 :p-2]}]
                                      (into [:div {:class [:h-100 :w-150 :p-2 :pl-2]}]
                                            (into (text->divs "Hiya! My name is JJ. I'm a 20 y/o computer science and electrical and computer engineering student at Worcester Polytechnic Institute. I'm doing a BS/MS in CS and a BS in ECE. I'm interested in hardware design as well as really low level software. I use any and all pronouns.
                                                               More specifically I like:")
                                                  [[:ul {:class [:list-disc :my-2 :pl-6]}
                                                    [:li "Async circuit design"]
                                                    [:li "RISC V Core design"]
                                                    [:li "Algorithmic music creation, including sample by sample creation"]
                                                    [:li "Graphics programming"]]]))]]])
                            (panel "Why?"
                                   [[(into [:div {:class [:w-222]}]
                                           (text->divs "This site was designed mainly as a resume and projects site for myself. I will probably put more in depth stuff about my projects here and will fill out the main page in the future. In the mean time, feel free to check out the other pages by clicking the top right button."))]])))}
   {:About (fn [state] [:div "This site was written in ClojureScript!"])}
   {:Projects (fn [state] (panel-container
                           (panel "Projects_List.txt"
                                  [[[:div "[*] - Done, linked"]
                                    [:div "[U] - Done, not written up"]
                                    [:div "[W] - Work in progress"]
                                    [:div "[A] - Abandoned"]
                                    [:div "[ ] - Unplanned"]]
                                   [(into [:div {:class [:w-222]}]
                                          (text->divs "╭ [A] Ray tracing first game engine (Pizza Box Engine)
                                                       ╰ [W] The rest of my projects lol. I need to populate this and it will be a lot of work"))]])
                           (panel "Pizza_Box_Engine.cpp"
                                  [[(into [:div {:class [:w-222]}]
                                          (text->divs "This was an attempt to make a ray tracing first game engine, with optimizations that only fully ray traced (as in VK_KHR_ray_tracing_pipeline instead of VK_KHR_ray_query in a fragment shader) rendering can provide.
                                               
                                                      I wanted to try to make a game engine for a while, and wanted to make a GPU-accelerated ray traced renderer for a while, and decided to combine the two. I realized that OpenGL did not have the required APIs to do hardware accelerated ray tracing, so I learnt Vulkan. I chose Vulkan and not DirectX for the cross platform nature, which would go on to be useful as I switched to Linux shortly after starting this project. I decided to do this in C++ because all the Vulkan examples and tutorials were in C++. I did not have enough graphics experience to use any other language or the best taste in languages. I also wanted to learn C++ because I had only ever did a small amount of C++ before and wanted to make a full complex project using it."))]
                                   [(into [:div {:class [:w-222]}]
                                          (text->divs "Eventually I did abandon it. I had started the project with not as much understanding of C++ as I should've and the technical debt from bad decisions the start of the project started becoming too annoying to continue working on it. C++ was a bad language to pick. I now know that C++ is just in general a bad language in my personal opinion. I also now know far more about graphics programming and now know that I made a few small mistakes that really would've caused problems in the future. I got as far as rendering one triangle using GPU accelerated ray tracing onto a Dear ImGUI panel as well as having a component system and a basic inspector."))]])))}])

(def page-names (doall (for [page config/site-definition] (s/replace (name (nth (keys page) 0))
                                                                     #"_" " "))))

(defn get-page-def [page func]
  (let [pageline (some #(get % (keyword (s/replace page #" " "_"))) site-definition)]
    (get pageline func)))


