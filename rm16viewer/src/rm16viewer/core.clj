(ns rm16viewer.core
  (:require [clojure.string    :as string]
            [clojure.java.io   :as io]
            [clojure.pprint    :as pp]
            [clj-time.core     :as time]
            [clj-time.format   :as format]
            [schema.core       :as s])
    (:use clj-xpath.core)
    (:import (java.io StringReader)
             (org.joda.time LocalDateTime LocalDate))
    (:gen-class))

(defn string-reader
  [s]
  (StringReader. s))

(def AEMO-RM16-DATE-FORMAT     (format/formatter "yyyy/MM/dd"))
(def AEMO-RM16-DATETIME-FORMAT (format/formatter "yyyy/MM/dd HH:mm:ss"))

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
   creation-dt     :- LocalDateTime
   settlement-date :- LocalDate
   hhr-data        :- [s/Num]])

(defn seq-of-bigdec
  [row-of-strings start-pos]
  (->> (nthrest row-of-strings start-pos)
       (drop-last)
       (map #(bigdec %1))))

(defn data->rm16row
  [row-of-strings]
  (->rm16-row
    (nth row-of-strings 0)
    (nth row-of-strings 1)
    (nth row-of-strings 2)
    (nth row-of-strings 3)
    (nth row-of-strings 4)
    (nth row-of-strings 5)
    (format/parse AEMO-RM16-DATETIME-FORMAT (nth row-of-strings 6))
    (format/parse AEMO-RM16-DATE-FORMAT (nth row-of-strings 7))
    (seq-of-bigdec row-of-strings 8)))

(defn construct-data
  []
  (->>
    (map #(string/split % #",") csv-payload)
    (rest)
    (map #(data->rm16row %1))))

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