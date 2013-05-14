(ns edn-play.json
  (:require [clojure.data.json :as json]
            [edn-play.flower :as flower]))

(defn data-to-file [file-name data]
  (spit file-name (json/write-str data)))

(defn write-flower-to-json-file [file-name]
  (data-to-file file-name flower/circle-set))

(defn write-tree-to-json-file [file-name]
  (data-to-file file-name flower/tree-set))

(defn write-metatron [file-name]
  (data-to-file file-name flower/metatron))

(defn write-tetrahedra [file-name]
  (data-to-file file-name flower/tetrahedra))

(defn write-cube [file-name]
  (data-to-file file-name flower/cube))

(defn write-octohedra [file-name]
  (data-to-file file-name flower/octohedra))

(defn read-json [file-name]
  (json/read-str (slurp file-name) :key-fn keyword))
