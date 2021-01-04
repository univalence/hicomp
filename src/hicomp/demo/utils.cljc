(ns hicomp.demo.utils
  (:require [hicomp.core :as h :refer [c]]
            [clojure.string :as str]
            #?(:clj  [clojure.pprint :as pp]
               :cljs [cljs.pprint :as pp])))

(defn pretty-str [x]
  (with-out-str (binding [pp/*print-right-margin* 50]
                  (pp/pprint x))))

(defn code-block [x & xs]
  (c :pre {:margin-bottom "0 !important"}
     (c :code.language-clojure
        x xs)))

(defn code-output [x]
  (c {:p 1 :margin {:bottom 2}
      :border [2 :#f7f7f7]}
     x))

(defn repl-output [x]
  (c :pre.repl-output
     {:margin-top "0 !important"
      :background-color "white !important"
      :border [2 :#f7f7f7]}
     (c :code.language-clojure
        x)))

#?(:clj
   (defmacro card [& content]
     `(c :.code-card
         (code-block ~(str/join "\n" (mapv pretty-str content)))
         (code-output (list ~@content)))))

#?(:clj
   (defmacro card* [& xs]
     `(c :.code-card
         (code-block ~(pretty-str (cons 'c xs)))
         (code-output (c ~@xs)))))

#?(:clj
   (defmacro expr [x]
     `(c :.code-card
         (code-block ~(pretty-str x))
         (repl-output (pretty-str ~x)))))

#?(:clj
   (defmacro code [& xs]
     `(c :.code-card
         (code-block ~@(interpose "\n" (map pretty-str xs)))
         (and ~@xs (repl-output ":ok")))))
