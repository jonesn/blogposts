(defproject rm16viewer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure              "1.6.0"]
                 [com.github.kyleburton/clj-xpath  "1.4.3"]
                 [prismatic/schema                 "0.4.3"]
                 [clj-time                         "0.10.0"]]

  :main ^:skip-aot rm16viewer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
