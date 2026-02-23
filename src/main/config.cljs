(ns config
  (:require
   [clojure.string :as s]
   [styling]
   [graphics]
   [projects.index :refer [project-pages]]
   [routing]
   [config-utils :as utils]))

(declare page-names)

(def site-definition
  [{:Main_Page {:render (fn [state store]
                          (utils/panel-container
                           (utils/panel "About Me"
                                        [[[:div {:class [:flex :flex-row]}
                                           [:div {:class [:size-100 :p-2]}]
                                           (into [:div {:class [:h-100 :w-150 :p-2 :pl-2]}]
                                                 (into (utils/text->divs "Hiya! My name is JJ. I'm a 20 y/o computer science and electrical and computer engineering student at Worcester Polytechnic Institute. I'm doing a BS/MS in CS and a BS in ECE. I'm interested in hardware design as well as really low level software. I use any and all pronouns.
                                                                          More specifically I like:")
                                                       [[:ul {:class [:list-disc :my-2 :pl-6]}
                                                         [:li "Async circuit design"]
                                                         [:li "RISC V Core design"]
                                                         [:li "Algorithmic music creation, including sample by sample creation"]
                                                         [:li "Graphics programming"]]]))]]])
                           (utils/panel "Why?"
                                        [[(into [:div {:class [:w-222]}]
                                                (utils/text->divs "This site was designed mainly as a resume and projects site for myself. I will probably put more in depth stuff about my projects here and will fill out the main page in the future. In the mean time, feel free to check out the other pages by clicking the top right button."))]])))}}
   {:About {:render (fn [state store] [:div "This site was written in ClojureScript!"])}}
   {:Projects {:render (fn [state store]
                         (let [page-args (:location/path (:selected-page state))]
                           (if-not page-args
                             (utils/panel-container
                              (utils/panel "Projects_List.txt"
                                           [[[:div "[*] - Done, linked"]
                                             [:div "[U] - Done, not written up"]
                                             [:div "[W] - Work in progress"]
                                             [:div "[A] - Abandoned"]
                                             [:div "[ ] - Unplanned"]]
                                            [(into [:div {:class [:w-222]}]
                                                   (utils/text->divs "╭ [A] Ray tracing first game engine (Pizza Box Engine)
                                                          ╰ [W] The rest of my projects lol. I need to populate this and it will be a lot of work"))]])
                              (utils/panel "Pizza_Box_Engine.cpp"
                                           [[(into [:div {:class [:w-222]}]
                                                   (utils/text->divs "This was an attempt to make a ray tracing first game engine, with optimizations that only fully ray traced (as in VK_KHR_ray_tracing_pipeline instead of VK_KHR_ray_query in a fragment shader) rendering can provide.
                                               
                                                      I wanted to try to make a game engine for a while, and wanted to make a GPU-accelerated ray traced renderer for a while, and decided to combine the two. I realized that OpenGL did not have the required APIs to do hardware accelerated ray tracing, so I learnt Vulkan. I chose Vulkan and not DirectX for the cross platform nature, which would go on to be useful as I switched to Linux shortly after starting this project. I decided to do this in C++ because all the Vulkan examples and tutorials were in C++. I did not have enough graphics experience to use any other language or the best taste in languages. I also wanted to learn C++ because I had only ever did a small amount of C++ before and wanted to make a full complex project using it."))]
                                            [(into [:div {:class [:w-222]}]
                                                   (utils/text->divs "Eventually I did abandon it. I had started the project with not as much understanding of C++ as I should've and the technical debt from bad decisions the start of the project started becoming too annoying to continue working on it. C++ was a bad language to pick. I now know that C++ is just in general a bad language in my personal opinion. I also now know far more about graphics programming and now know that I made a few small mistakes that really would've caused problems in the future. I got as far as rendering one triangle using GPU accelerated ray tracing onto a Dear ImGUI utils/panel as well as having a component system and a basic inspector."))]]))
                             (if-let [project-page (first (filter #(= (:id %) (keyword page-args)) project-pages))]
                               ((:render (:callbacks project-page)) state store)
                               [:div {:class [:flex :justify-center]}
                                [:div {:class [:flex :flex-col
                                               :gap-4 :w-fit]}
                                 [:div {:class [:text-xl :font-black]} "Project Directory"]
                                 (into [:ul {:class [:list-disc]}]
                                       (map (fn [proj-page] [:li
                                                             [:div {:on {:click (fn [e]
                                                                                  (routing/jump-to (str "#/Projects/" (name (:id proj-page))) page-names store))}}
                                                              (:name proj-page)]]) project-pages))]]))))
               :post-render (fn [state store]
                              (when-let [page-args (:location/path (:selected-page state))]
                                (when-let [project-page (first (filter #(= (:id %) (keyword page-args)) project-pages))]
                                  ((:post-render (:callbacks project-page)) state store))))}}])
   ;{:Project_Pages {:render (fn [state store] [:div {:class [:flex :justify-center]}])}}])
           ;:post-render (fn [state store] (graphics/create-shader-canvas "canvas-1" "vec3(y, x, f32(u32(y * uniforms.resolution_y) ^ u32(x * uniforms.resolution_x)))" {:width 256 :height 256}))}}])

(def page-names (doall (for [page config/site-definition] (s/replace (name (nth (keys page) 0))
                                                                     #"_" " "))))

(defn get-page-def [page func]
  (let [pageline (some #(get % (keyword (s/replace page #" " "_"))) site-definition)]
    (get pageline func)))


