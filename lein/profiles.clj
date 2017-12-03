{:user {:plugins [[lein-cljfmt "0.5.7"]]
        :dependencies [[cljfmt "0.5.1"]
                       [jonase/eastwood "0.2.1"
                        :exclusions [org.clojure/clojure]]]
        :repl-options {:init (require 'cljfmt.core)}}}
