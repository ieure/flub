(defproject flub "0.1.1-SNAPSHOT"
  :description "The Fluke Utility Box"
  :url "http://github.com/ieure/flub"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-mmap "1.1.2"]
                 [com.taoensso/timbre "3.2.1"]
                 [instaparse "1.3.2"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/tools.macro "0.1.2"]
                 [pjstadig/humane-test-output "0.6.0"]
                 [slingshot "0.10.3"]]
  :warn-on-reflection true
  :aot :all
  :jvm-opts ["-Xmx1g" "-Xms1g"]
  :main flub.core
  :resource-paths ["resources/"]
  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]
  :profiles {:dev {:resource-paths ["examples/"
                                    "resources/"
                                    "test-resources/"]
                   :plugins [[lein-cloverage "1.0.2"]]}})
