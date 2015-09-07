(ns rm16viewer.core
  (:require [clojure.string    :as string]
            [clojure.java.io   :as io]
            [clojure.pprint    :as pp]
            [clj-time.format   :as format]
            [schema.core       :as s]
            [clojure.tools.cli :refer [parse-opts]])
    (:use clj-xpath.core)
    (:import (java.io StringReader)
             (org.joda.time LocalDateTime LocalDate))
    (:gen-class))

;; =========
;; Constants
;; =========

(defn string-reader
  [s]
  (StringReader. s))

(def AEMO-RM16-DATE-FORMAT     (format/formatter "yyyy/MM/dd"))
(def AEMO-RM16-DATETIME-FORMAT (format/formatter "yyyy/MM/dd HH:mm:ss"))

;; ==========
;; Data Types
;; ==========

;; This record represents the typed representation of a CSV row from the AEMO file.
(s/defrecord RM16Row
  [tni             :- String
   data-type       :- String
   frmp            :- String
   lr              :- String
   mdp             :- String
   profile-name    :- String
   creation-dt     :- LocalDateTime
   settlement-date :- LocalDate
   hhr-data        :- [s/Num]])

(s/defrecord RM16Summary
  [profileToGWh      :- {:profile String :volume Number}])

(defn seq-of-bigdec-from-csv-chunk
  [row-of-strings start-pos]
  (->> (nthrest row-of-strings start-pos)
       (drop-last)
       (map #(bigdec %1))))

(defn csvdata->rm16row
  [row-of-strings]
  ;; Validate will return the object or throw an exception if the
  ;; item doesn't validate
  (s/validate RM16Row
    (->RM16Row
      (nth row-of-strings 0)
      (nth row-of-strings 1)
      (nth row-of-strings 2)
      (nth row-of-strings 3)
      (nth row-of-strings 4)
      (nth row-of-strings 5)
      (format/parse-local           AEMO-RM16-DATETIME-FORMAT (nth row-of-strings 6))
      (format/parse-local-date      AEMO-RM16-DATE-FORMAT     (nth row-of-strings 7))
      (seq-of-bigdec-from-csv-chunk row-of-strings 8))))

;; =============
;; Parsing Logic
;; =============

(defn parse-rm16-doc
  [file-path]
  (xml->doc (slurp file-path)))

(defn extract-csv-payload
  [rm16-doc]
  (line-seq
    (io/reader
      (string-reader ($x:text "//CSVData" rm16-doc)))))

(defn construct-rm16-data
  [csv-payload]
  (->>
    (map #(string/split % #",") csv-payload)
    (rest)
    (map #(csvdata->rm16row %1))))

(defn sum-volume-for-profile
  [profile-grouped-map-element]
  (let [profile    (key profile-grouped-map-element)
        sum-volume (reduce + (mapcat #(:hhr-data %1)
                                     (val profile-grouped-map-element)))]
    {profile sum-volume}))

(defn construct-gwh-per-profile
  [seq-of-rm16row]
  (->>
    (group-by #(:profile-name %1) seq-of-rm16row)
    (map      #(sum-volume-for-profile %1))
    (reduce   merge)))

(defn process-file
  [file-path]
  (->>
    (parse-rm16-doc file-path)
    (extract-csv-payload)
    (construct-rm16-data)
    (construct-gwh-per-profile)))

;; =====================
;; Command Line and Main
;; =====================

(defn exit [status msg]
  (println msg)
  (System/exit status))

(def command-line-schema
  [["-h" "--help"]])

(defn usage
  [options-summary]
  (->> ["Summarize the Volume in an AEMO RM16 File."
        ""
        "Usage: rm16viewer fileName"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn -main
  [& args]
  (let [cli-params (parse-opts args command-line-schema)
        options    (:options cli-params)
        summary    (:summary cli-params)
        arguments  (:arguments cli-params)]
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 1) (exit 1 (usage summary))
      :default (process-file (first arguments)))))