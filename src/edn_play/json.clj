(ns edn-play.json
  (:require [clojure.data.json :as json]
            [edn-play.flower :as flower]))

(defn write-flower-to-json-file [file-name]
  (spit file-name  (json/write-str flower/circle-set)))

(defn write-tree-to-json-file [file-name]
  (spit file-name  (json/write-str flower/tree-set)))

(defn read-json [file-name]
  (json/read-str (slurp file-name) :key-fn keyword))
