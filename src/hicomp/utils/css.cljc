(ns hicomp.utils.css
  #?(:cljs (:require-macros [hicomp.utils.css :refer [calc]])))

(defn size [x]
  (cond (number? x) (str x "px")
        (string? x) x
        (or (keyword? x) (symbol? x)) (name x)
        :else (throw (#?(:cljs js/Error.
                         :clj  Exception.) (str "not a valid css size: " x)))))

(defn calc-expr [expr]
  (cond
    (and (seq? expr) ('#{+ / * -} (first expr)))
    (mapcat calc-expr
            (interpose (str " " (first expr)) (next expr)))
    :else [" " expr]))

#?(:clj
   (defmacro calc [expr]
     `(str "calc(" ~@(calc-expr expr) ")")))

(defn sum-calc [x]
  (str "calc("
       (apply str (interpose " + " x))
       ")"))