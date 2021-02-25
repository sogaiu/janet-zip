(import ./janet-zip/vendor/path)

(declare-project
 :name "janet-zip"
 :url "https://github.com/sogaiu/janet-zip"
 :repo "git+https://github.com/sogaiu/janet-zip.git")

(def proj-root
  (os/cwd))

(def src-root
  (path/join proj-root "janet-zip"))

(declare-source
 :source [(path/join src-root "zip.janet")])

(phony "netrepl" []
       (os/execute
        ["janet" "-e" (string "(os/cd \"" src-root "\")"
                              "(import spork/netrepl)"
                              "(netrepl/server)")] :p))

