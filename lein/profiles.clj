{:user {;:pedantic? :warn
        :plugins [[cider/cider-nrepl "0.18.0"]]
        :dependencies [[acyclic/squiggly-clojure "0.1.8"
                        :exclusions [org.clojure/tools.reader]]
                       [org.clojure/tools.nrepl "0.2.13"
                        :exclusions [org.clojure/clojure]]
                       [org.clojure/core.cache "0.6.5"]]
        }
 }
