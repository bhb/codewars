(defproject codewars "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :profiles {:dev {:dependencies [[alembic "0.3.2"]
                                  [org.clojure/test.check "0.6.1"]
                                  [criterium "0.4.3"]
                                  [com.taoensso/timbre "3.4.0"]
                                  ]}})
