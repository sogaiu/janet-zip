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

# XXX: the following can be used to arrange for the overriding of the
#      "test" phony target -- thanks to rduplain and bakpakin
(put (dyn :rules) "test" nil)
(phony "test" ["build"]
       (os/execute ["jg-verdict"
                    "-p" proj-root
                    "-s" src-root] :p))

(phony "judge" ["build"]
       (os/execute ["jg-verdict"
                    "-p" proj-root
                    "-s" src-root] :p))
