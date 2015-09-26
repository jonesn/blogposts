(ns rm16viewer.core
  (:require [clojure.string    :as string]
            [clojure.java.io   :as io]
            [clj-time.format   :as format]
            [schema.core       :as s]
            [clojure.tools.cli :refer [parse-opts]])
    (:use clj-xpath.core)
    (:import (java.io StringReader File)
             (org.joda.time LocalDateTime LocalDate))
    (:gen-class))

;; =====================
;; Constants and Helpers
;; =====================

(defn string-reader
  [s]
  (StringReader. s))

(defn file-exists?
  [path]
  (.exists (File. path)))

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

;; The summary data to be printed.
(s/defrecord RM16Summary
  [jurisdiction      :- String
   profile           :- String
   volume            :- BigDecimal])

(defn rm16-summary-compare
  [rm16-summary-a rm16-summary-b]
  (compare
    (str (:jurisdiction rm16-summary-a) (:profile rm16-summary-a))
    (str (:jurisdiction rm16-summary-b) (:profile rm16-summary-b))))

(defn seq-of-bigdec-from-csv-chunk
  [row-of-strings start-pos]
  (->> (nthrest row-of-strings start-pos)
       (drop-last)
       (map bigdec)))

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
  (if (file-exists? file-path)
    (xml->doc (slurp file-path))
    (do
      (println (str "File " file-path " not found."))
      nil)))

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

;; ====================
;; Transformation Logic
;; ====================

(defn sum-volume-for-jurisdiction-profile
  [rm16-summary-to-row-seq]
  (let [sum-volume   (/ (reduce +
                                (mapcat :hhr-data
                                        (val rm16-summary-to-row-seq)))
                        1000)]
    (s/validate RM16Summary
      (->RM16Summary (:jurisdiction (key rm16-summary-to-row-seq))
                     (:profile      (key rm16-summary-to-row-seq))
                     sum-volume))))

(defn jurisdiction-profile-key
  [rm16-row]
  (let [tni-lead-char (str (first (:tni rm16-row)))
        profile       (:profile-name rm16-row)
        jurisdiction (case tni-lead-char
                       "V" "VIC"
                       "N" "NSW"
                       "A" "ACT"
                       "Q" "QLD"
                       "S" "SA")]
    (->RM16Summary jurisdiction profile 0)))

(defn construct-mwh-per-jurisdiction-per-profile
  [seq-of-rm16row]
  (->>
    (group-by jurisdiction-profile-key seq-of-rm16row)
    (map      sum-volume-for-jurisdiction-profile)))

(defn print-summary
  [seq-of-rm16-summary]
  (let [total-volume (reduce + (map #(:volume %) seq-of-rm16-summary))]
    (doseq [summary seq-of-rm16-summary]
      (printf "Jurisdiction: %5s   Profile: %12s   Volume MWh: %13s \n"
              (:jurisdiction summary)
              (:profile       summary)
              (:volume        summary)))
    (printf "Total MWh: %10s" total-volume)
    (flush)))

(defn process-file
  [file-path]
  ;; Some will continue to thread for non nil values
  (some->>
    (parse-rm16-doc file-path)
    (extract-csv-payload)
    (construct-rm16-data)
    (construct-mwh-per-jurisdiction-per-profile)
    (sort rm16-summary-compare)
    (print-summary)))

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
      (:help options)            (exit 0 (usage summary))
      (not= (count arguments) 1) (exit 1 (usage summary))
      :default                   (process-file (first arguments)))))