(defproject tapp "0.1.0"
  :description "Tapp tarock playing engine"
  :url "https://github.com/erkin/tapp-tarock"
  :license {:name "MPL-2.0"
            :url "https://www.mozilla.org/en-US/MPL/2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot tapp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
