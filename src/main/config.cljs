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
  [:div {:class [:w-222 :h-fit]}
   [:div {:class [:w-full :h-fit (styling/color-tag "border" :highlight) :border-3 :mb-1
                  :rounded-tr-lg :rounded-bl-lg]
          :style {:corner-shape "notch"}}
    [:div {:class [:mx-3 :my-1 :underline :font-bold]}
     name]]
   (for [panel-contents content]
     (into [:div {:class [:w-full :h-fit (styling/color-tag "border" :highlight) :border-3
                          :px-3 :py-1 :my-1
                          :rounded-tr-lg :rounded-bl-lg :text-sm]
                  :style {:corner-shape "notch"}}]
           panel-contents))])

(defn text->divs [text]
  (map (fn [line] [:p (clojure.string/trim line)])
       (clojure.string/split-lines text)))

(def site-definition
  [{:Main_Page {:render (fn [state] [:div "This site was designed mainly as a resume and projects site for myself. I will probably put more in depth stuff about my projects here and will fill out the main page in the future. In the mean time, feel free to check out the other pages by clicking the top right button."])}}
   {:About {:render (fn [state] [:div "This site was written in ClojureScript!"])}}
   {:Projects {:render (fn [state] (panel-container
                                    (panel "Projects_List.txt"
                                           [[[:div "[*] - Done, linked"]
                                             [:div "[U] - Done, not written up"]
                                             [:div "[W] - Work in progress"]
                                             [:div "[A] - Abandoned"]
                                             [:div "[ ] - Unplanned"]]
                                            (text->divs "
                                               ╭ [A] Ray tracing first game engine (Pizza Box Engine)
                                               ╰ [W] The rest of my projects lol. I need to populate this and it will be a lot of work")])
                                    (panel "Pizza_Box_Engine.cpp"
                                           [(text->divs "This was an attempt to make a ray tracing first game engine, with optimizations that only fully ray traced (as in VK_KHR_ray_tracing_pipeline instead of VK_KHR_ray_query in a fragment shader) rendering can provide.
                                               
                                               I wanted to try to make a game engine for a while, and wanted to make a GPU-accelerated ray traced renderer for a while, and decided to combine the two. I realized that OpenGL did not have the required APIs to do hardware accelerated ray tracing, so I learnt Vulkan. I chose Vulkan and not DirectX for the cross platform nature, which would go on to be useful as I switched to Linux shortly after starting this project. I decided to do this in C++ because all the Vulkan examples and tutorials were in C++. I did not have enough graphics experience to use any other language or the best taste in languages. I also wanted to learn C++ because I had only ever did a small amount of C++ before and wanted to make a full complex project using it.")
                                            (text->divs "Eventually I did abandon it. I had started the project with not as much understanding of C++ as I should've and the technical debt from bad decisions the start of the project started becoming too annoying to continue working on it. C++ was a bad language to pick. I now know that C++ is just in general a bad language in my personal opinion. I also now know far more about graphics programming and now know that I made a few small mistakes that really would've caused problems in the future. I got as far as rendering one triangle using GPU accelerated ray tracing onto a Dear ImGUI panel as well as having a component system and a basic inspector.")])))}}
   {:Test {:render (fn [state] [:div {:class [:flex :justify-center]}
                                (graphics/shader-canvas-hiccup "canvas-1" 256 256)])
           :post-render (fn [state] (graphics/create-shader-canvas "canvas-1" "vec3(y, x, f32(u32(y * uniforms.resolution_y) ^ u32(x * uniforms.resolution_x)))" {:width 256 :height 256}))}}])

(def page-names (doall (for [page config/site-definition] (s/replace (name (nth (keys page) 0))
                                                                     #"_" " "))))

(defn get-page-def [page func]
  (let [pageline (some #(get % (keyword (s/replace page #" " "_"))) site-definition)]
    (get pageline func)))


