(ns rm16viewer.core
  (:require [clojure.string    :as string]
            [clojure.java.io   :as io]
            [clojure.pprint    :as pp]
            [clj-time.core     :as time]
            [schema.core       :as s])
    (:use clj-xpath.core)
    (:import (java.io StringReader)
             (org.joda.time DateTime))
    (:gen-class))

(defn string-reader
  [s]
  (StringReader. s))

(def rm16-doc
  (xml->doc (slurp "resources/mdmtl_contactabatch_569104818.xml")))

(def csv-payload
  (line-seq
    (io/reader
      (string-reader ($x:text "//CSVData" rm16-doc)))))

(s/defrecord rm16-row
  [tni             :- String
   data-type       :- String
   frmp            :- String
   lr              :- String
   mdp             :- String
   profile-name    :- String
   creation-dt     :- DateTime
   settlement-date :- DateTime
   hhr-data        :- [s/Num]])

(defn construct-data
  []
  (->>
    (map #(string/split % #",") csv-payload)
    (map #(->rm16-row %1 %2 %3 %4 %5 %6 %7 %8 []))))

(defn visit-nodes
  ([path nodes f]
     (vec
      (mapcat
       #(vec
         (cons
          ;; invoke the callback on the each of the nodes
          (f (conj path (:tag %1)) %1)
          ;; visit each of the children of this node
          (visit-nodes
           (conj path (:tag %1))
           ($x "./*" %1) f)))
       nodes))))

(defn all-paths [doc]
  (map
   #(str "/" (string/join "/" (map name %1)))
   (first
    (reduce
     (fn [[acc set] p]
       (if (contains? set p)
         [acc set]
         [(conj acc p) (conj set p)]))
     [[] #{}]
     (visit-nodes []
                  ($x "./*" doc)
                  (fn [p n]
                    p))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
