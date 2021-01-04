(ns hicomp.utils
  (:require
    [clojure.string :as str]
    [clojure.walk :refer [postwalk]]
    [#?(:cljs cljs.pprint :clj clojure.pprint) :as pp])
  #?(:cljs (:require-macros [hicomp.utils :refer [defn+]])))

;; printlng debugging
;; ------------------------------------------------------------

(defn pp [& xs]
  (mapv pp/pprint xs))

(defn prob
  "print all its arguments and return the last"
  [& xs]
  (apply pp xs) (last xs))

#?(:cljs (defn log [& xs]
           (mapv #(js/console.log %) xs)))

(defn error [& xs]
  (throw (#?(:cljs js/Error
             :clj  Exception.)
           (apply str xs))))

;; DOM
;; ------------------------------------------------------------

#?(:cljs

   (do
     (defn $1 [x]
       (js/document.querySelector x))

     (defn $ [x]
       (array-seq (js/document.querySelectorAll x)))

     (defn tval [e]
       (.. e -target -value))))

;; words
;; ------------------------------------------------------------

(defn word? [x]
  (or (string? x)
      (symbol? x)
      (keyword? x)))

(defn mksym [& xs]
  (->> xs (map name) (apply str) symbol))

;; macros
;; ------------------------------------------------------------

#?(:clj
   (defmacro f1 [pat & body]
     `(fn [~pat] ~@body)))

#?(:clj
   (defmacro f_ [& body]
     `(fn [~'_] ~@body)))

#?(:clj
   (defmacro defn+
     "behave the same as defn but will also define applied and underscore variations"
     [name & body]
     (let [name* (mksym name '*)
           name_ (mksym name '_)
           name_* (mksym name '_*)]
       `(do (declare ~name* ~name_ ~name_*)
            (defn ~name ~@body)
            (def ~name* (partial apply ~name))
            (defn ~name_ [& xs#] #(~name* % xs#))
            (def ~name_* (partial apply ~name_))))))

;; numbers
;; --------------------------------------------------------------------

(defn parse-int [x]
  (cond
    (int? x) x
    (or (string? x) (keyword? x))
    (#?(:cljs js/parseInt
        :clj  java.lang.Integer/parseInt) (name x))))

(defn geometric-scale [base delta]
  (let [up (next (iterate #(* % (+ 1 delta)) base))
        down (next (iterate #(* % (- 1 delta)) base))]
    (fn [i]
      (cond (zero? i) base
            (pos? i) (nth up (dec i))
            (neg? i) (nth down (dec (- i)))))))

;; collection
;; ---------------------------------------------------------------------

(defn link [a b]
  #?(:clj  (clojure.lang.MapEntry. a b)
     :cljs (cljs.core/MapEntry. a b nil)))

(def link? map-entry?)

(defn unrecordify [x]
  (clojure.walk/prewalk
    #(if (record? %) (into {} %) %) x))

(defn deep-merge
  ([x y]
   (cond
     (nil? x) y
     (nil? y) x

     (every? map? [x y])
     (merge-with deep-merge x y)

     (every? set? [x y])
     (into x y)

     :else y))
  ([x y & ys]
   (reduce deep-merge
           x (cons y ys))))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn map-h [f m]
  (into {} (map (fn [e] (f (key e) (val e))) m)))

(defn- upd1 [x [p f]]
  (if (sequential? p)
    (update-in x p f)
    (update x p f)))

(defn+ upd
       "like update/update-in but with the possibility to give several updates
        it handles sequentials paths with update-in and non sequentials with update so you can mix deep and shallow paths"
       [x & xs]
       (reduce upd1 x (partition 2 xs)))

(defn- put1 [x [p v]]
  (if (sequential? p)
    (assoc-in x p v)
    (assoc x p v)))

(defn+ put
       "like assoc/assoc-in but with the possibility to give several associations"
       [x & xs]
       (reduce put1 x (partition 2 xs)))

(defn sorted-int-map [x]
  (into (sorted-map-by >)
        (map-keys parse-int x)))

;; stolen from https://github.com/madvas/cljs-react-material-ui
(defn transform-keys [t coll]
  "Recursively transforms all map keys in coll with t."
  (letfn [(transform [[k v]] [(t k) v])]
    (postwalk (fn [x] (if (map? x) (into {} (map transform x)) x)) coll)))

(defn unqualifed-keyword [x]
  (keyword (name x)))

(defn unqualified-keys
  "recursively remove all namespaces from all keys"
  [x]
  (transform-keys
    (fn [k] (if (keyword? k) (keyword (name k)) k))
    x))

(defn remove-nil-vals [x]
  (into {} (remove (comp nil? val) x)))

(defn get-flags [m]
  (map key (filter (comp true? val) m)))

(defn set_toggle [s x]
  ((if (s x) disj conj) s x))

;; misc
;; -----------------------------------------------------------------

(defn parse-fn [[fst & nxt :as all]]

  (let [[name fst & nxt]
        (if (symbol? fst)
          (cons fst nxt)
          (concat [nil fst] nxt))

        [doc fst & nxt]
        (if (string? fst)
          (cons fst nxt)
          (concat [nil fst] nxt))

        [opts fst & nxt]
        (if (map? fst)
          (cons fst nxt)
          (concat [{} fst] nxt))

        impls
        (if (vector? fst)
          {fst (vec nxt)}
          (into {}
                (map
                  (fn [[args & body]]
                    [args (vec body)])
                  (cons fst nxt))))]

    (assoc opts
      :name name
      :doc doc
      :impls impls
      :cases (mapv (partial apply list*) impls))))

#?(:clj
   (defmacro marked-fn

     "marked function,
      define an anonymous form (like fn)
      a def form (like defn)
      and a predicate function (like fn?)"

     [name & [doc]]

     `(do

        (defmacro ~name
          [& body#]
          (let [parsed# (parse-fn body#)]
            `(with-meta
               (fn ~(or (:name parsed#) (gensym)) ~@(:cases parsed#))
               {~~(keyword name) true})))

        (defn ~(mksym name "?") [x#]
          (when (-> x# meta ~(keyword name)) x#))

        (defmacro ~(mksym 'def name) [name'# & body#]
          `(def ~name'# (~'~name ~@body#))))))

(defn guard [f]
  (fn [x & xs]
    (when (apply f x xs) x)))

(defmacro promise->
  [promise & body]
  `(.catch
     (-> ~promise
         ~@(map (fn [expr] (list '.then expr)) body))
     (fn [error#]
       (prn "Promise rejected " {:error error#}))))






