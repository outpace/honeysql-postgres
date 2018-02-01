(defproject nilenso/honeysql-postgres "0.2.4"
  :description "PostgreSQL extension for honeysql"
  :url "https://github.com/nilenso/honeysql-postgres"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.339" :scope "provided"]
                 [org.clojure/test.check "0.10.0-alpha2" :scope "test"]
                 [honeysql "0.9.3"]]
  :doo {:build "test"
        :alias {:default [:node]}}
  :cljsbuild {:builds {:test {:source-paths ["resources" "src" "test"]
                              :compiler {:output-to "target/doo/honeysql-postgres.js"
                                         :output-dir "target/doo"
                                         :optimaztions :none
                                         :main honeysql-postgres.test
                                         :target :nodejs
                                         :parallel-build true}}}}
  :tach {:test-runner-ns 'honeysql-postgres.postgres-test
         :source-paths ["src" "test"]}
  :profiles {:dev {:plugins [[lein-tach "1.0.0"]
                             [lein-cljsbuild "1.1.7"]
                             [lein-doo "0.1.10"]]}})
