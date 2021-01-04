(ns hicomp.styles.dimensions
  (:require [hicomp.utils :as u]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hicomp.styles.dimensions :refer [calc]])))

(def scale-aliases
  {:base 0
   :lg 1 :xl 2 :xxl 3 :3xl 4 :4xl 5
   :sm -1 :xs -2 :xxs -3 :3xs -4 :4xs -5})

(def default-scales {:space {:base 0.5 :delta 0.5}
                     :font {:base 1 :delta 0.25}})

(defn default-scale [k]
  (when-let [{:keys [base delta]} (default-scales k)]
    (u/geometric-scale base delta)))

(def ratio-aliases
  {:full 1
   :double 2 :triple 3 :quad 4
   :half 0.5 :third (/ 1 3) :fourth 0.25 :fifth (/ 1 5)
   :sixth (/ 1 6) :seventh (/ 1 7) :eighth 0.125 :ninth (/ 1 9) :tenth 0.1})

;; TODO
(defn validate-css-raw-dimension [x] x)

(defn size
  "size is a way to define easily width and height values, it uses % unit"
  [x]
  (cond (number? x) (str x "%")
        (contains? ratio-aliases x) (size (* 100 (ratio-aliases x)))
        (or (ident? x) (string? x)) (validate-css-raw-dimension x)
        :else (u/error "not a valid css size")))

(defn space
  "space is a way to define easily padding and margin values, it uses rem unit"
  [x & [scale]]
  (cond (number? x) (str (* (get-in default-scales [:space :base]) x) "rem")
        (contains? ratio-aliases x) (space (ratio-aliases x))
        (contains? scale-aliases x) (space ((or scale (default-scale :space)) (scale-aliases x)))
        (or (ident? x) (string? x)) (validate-css-raw-dimension x)
        :else (u/error "not a valid css space")))

(defn font
  "font is a way to define easily font-size values, it uses rem unit"
  [x & [scale]]
  (cond (number? x) (str (* (get-in default-scales [:font :base]) x) "rem")
        (contains? ratio-aliases x) (font (ratio-aliases x))
        (contains? scale-aliases x) (font ((or scale (default-scale :font)) (scale-aliases x)))
        (or (ident? x) (string? x)) (validate-css-raw-dimension x)
        :else (u/error "not a valid css font size")))

(comment
  (size :full)
  (size (/ 2 3))
  (size "3rem")
  (size nil)

  (font :half)
  (font :xl)

  (space 0.7)
  (space "9px"))

(defn sexp->infix-str [op args]
  (str "(" (str/join (str " " (name op) " ") args) ")"))

(defn calc-expr [f expr]
  (if (seq? expr)
    (cond
      ;; for div and mult only the first argument is submited to the given f
      ;; e.g subsequent args have to be numbers
      ('#{/ *} (first expr))
      (sexp->infix-str
        (first expr)
        (cons (calc-expr f (second expr))
              (map (partial calc-expr identity) (drop 2 expr))))
      ;; for plus and minus all args can be css units, so we just recur on every args
      ('#{+ -} (first expr))
      (sexp->infix-str
        (first expr)
        (map (partial calc-expr f) (rest expr))))
    (f expr)))

(def type->fn
  {:size size
   :space space
   :font font})

#?(:clj
   (defmacro calc [type expr]
     `(str "calc" ~@(calc-expr (type->fn type) expr))))

(comment
  (calc :space (- :full (/ :xxs 10) (- (* :xl 2) "1px"))))

(defmacro define-space-mixin

  [nam css-prefix short-name]

  `(do (defn ~nam
         ([x#]
          (if (map? x#)
            (->> x#
                 (u/map-keys (fn [k#] (keyword (str ~(name css-prefix) "-" (name k#)))))
                 (u/map-vals space))
            (~nam {:top x#
                   :bottom x#
                   :left x#
                   :right x#})))

         ([x# y#]
          (~nam {:top y#
                 :bottom y#
                 :left x#
                 :right x#}))

         ([t# r# b# l#]
          (~nam {:top t#
                 :bottom b#
                 :left l#
                 :right r#})))

       (def ~short-name ~nam)

       ~@(for [[n dirs] [["" [:right :left :top :bottom]]
                         ["x" [:right :left]]
                         ["y" [:top :bottom]]
                         ["t" [:top]]
                         ["b" [:bottom]]
                         ["l" [:left]]
                         ["r" [:right]]]
               size (range 5)]
           `(def ~(u/mksym short-name n (str size))
              (~nam ~(reduce #(assoc %1 %2 `(space ~size)) {} dirs))))))

(macroexpand-1 '(define-space-mixin margin margin m))