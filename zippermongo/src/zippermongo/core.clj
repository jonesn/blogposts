(ns zippermongo.core
    (:require [monger.core :as mg]
              [monger.collection :as col]
              [clojure.zip :as zip])
    (use clojure.pprint))

;; 1. This is the Hiccup we will store and return from Mongo.

(def example-hiccup
     [:body
      [:p "Hello World Paragraph"]
      [:ul
       [:li "Clojure"]
       [:li "Mongo"]
       [:li "Hiccup"]]])

;; 2. Connect to the local MongoDB Instance.

(mg/connect! {:host "localhost"
              :port 27017})
(mg/set-db! (mg/get-db "zipper-mongo-blog"))

;; 3. Define a collection to store our hiccup in.

(def HICCUP_COLLECTION "hiccupcollection")

;; 4. Write our Hiccup to Mongo.

(defn write-data-to-mongo
  [name hiccup]
  (col/insert HICCUP_COLLECTION {:name name :hiccup hiccup}))

;; 5. Read the Hiccup back from Mongo

(defn read-data-from-mongo
  [name]
  (col/find-one-as-map HICCUP_COLLECTION {:name name}))

;; 6. Use Clojure Zippers to walk the hiccup content and keywordize the first element of each vector.

(defn keyword-vector-head
  "Changes the first element of the vector to a keyword and returns a vector."
  [vec]
  (apply vector
   (cons (keyword (first vec))
         (rest vec))))

(defn deserialize-hiccup-content
  "Deserialize the stored hiccup content for use by Clojure."
  [hiccup-content]
  (loop [hiccup-tree (zip/vector-zip hiccup-content)]
    ;; If we are at the end of the tree then return its root.
    (cond (zip/end? hiccup-tree) (zip/root hiccup-tree)
          :else
          ;; If our current node is a vector then replace its head with a keyword and return the location of the edited node.
          ;; otherwise continue traversal.
          (let [next-loc (cond (vector? (zip/node hiccup-tree)) (zip/edit hiccup-tree keyword-vector-head)
                               :else hiccup-tree)]
            (recur (zip/next next-loc))))))
