(defproject flub "0.1.1-SNAPSHOT"
  :description "The Fluke Utility Box"
  :url "http://github.com/ieure/flub"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.macro "0.1.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [slingshot "0.10.3"]
                 [clj-mmap "1.1.2"]
                 [instaparse "1.2.12"]]
  :warn-on-reflection true
  :aot :all
  :jvm-opts ["-Xmx1g" "-Xms1g"
             ;; "-XX:+PrintGC" "-XX:+PrintGCDetails" "-XX:+PrintGCTimeStamps"
             ]
  :main flub.core
  :profiles {:dev {:resource-paths ["examples/" "resources/"]}})


