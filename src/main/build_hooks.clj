(ns build-hooks
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn filename->namespace [filename]
  (-> filename
      (str/replace #"\.cljs$" "")
      (str/replace #"_" "-")
      (str/replace #"/" ".")
      (str/replace #"-" "-")))

(defn generate-project-index
  {:shadow.build/stage :compile-prepare}
  [build-state]
  (let [projects-dir (io/file "src/main/projects")
        project-files (->> projects-dir
                           file-seq
                           (filter #(.isFile %))
                           (filter #(re-matches #".*\.cljs$" (.getName %)))
                           (remove #(= "index.cljs" (.getName %))))

        ;; Generate namespace requires
        namespaces (map (fn [f]
                          (let [rel-path (str/replace (.getPath f)
                                                      (str (.getPath projects-dir) "/")
                                                      "")
                                ns-name (str "projects." (filename->namespace rel-path))
                                alias (str "p" (hash ns-name))]
                            {:ns ns-name :alias alias}))
                        project-files)

        ;; Generate the index file content
        requires-str (str/join "\n            "
                               (map #(format "[%s :as %s]" (:ns %) (:alias %))
                                    namespaces))
        configs-str (str/join "\n   "
                              (map #(str (:alias %) "/config")
                                   namespaces))

        index-content (format
                       "(ns projects.index
  (:require %s))

(def project-pages
  [%s])
"
                       requires-str
                       configs-str)

        output-file (io/file ".shadow-cljs/generated/projects/index.cljs")]

    (io/make-parents output-file)
    (spit output-file index-content)
    (println "Generated project index with" (count namespaces) "projects")
    build-state))
