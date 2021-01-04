(ns hicomp.xp.comp
  (:require [stylefy.core :as stylefy]
            [hicomp.styles.mixins :as s]
            [hicomp.utils :as u]
            [clojure.string :as str]))

(u/marked-fn cfn
           "a marked function to denote component updates")

;; NOT USED experimental...

(defn cfn?
  "cfn represent a component transformation
   see cfn macro in the corresponding clj namespace"
  [x]
  (some-> x meta :cfn))

(defn ->cfn
  "turn a regular lambda into a cfn"
  [fn]
  (vary-meta fn merge {:cfn true}))

(comment
  ;; short form
  (= 2 ((cfn (+ _ _)) 1))
  ;; from lambda
  (= 2 ((->cfn inc) 1))
  ;; with sym pattern
  (= 2 ((cfn x (* 2 x)) 1))
  ;; with destr pattern
  (= 2 ((cfn {a :a} (inc a)) {:a 1}))
  ;; with recursion
  (= :done
     ((cfn rec x (if (zero? x) :done (rec (dec x))))
      10)))

;; impl
;; -----------------------------------------------------------

(defn ++-old
  "combinate two things in the context of a component
   vectors denotes left to right composition
   maps are deeply merged
   cfn objects denotes data transform/abstraction (nested or not)
   seqs denotes aggregation"
  ([x y]

   (cond
     (nil? x) y
     (nil? y) x

     (every? map? [x y])
     (merge-with ++ x y)

     (every? cfn? [x y])
     (->cfn (comp y x))

     (vector? x)
     (++ (reduce ++ x) y)

     (vector? y)
     (reduce ++ x y)

     (cfn? y) (y x)

     (seq? x)
     (concat x (if (seq? y) y [y]))

     :else y))

  ([x y & xs]
   (reduce ++ (++ x y) xs)))

(declare lens)

(defn ++
  "combinate two things in the context of a component
   vectors denotes left to right composition
   maps are deeply merged
   cfn objects denotes data transform/abstraction (nested or not)
   seqs denotes aggregation"

  ([x] (partial ++ x))

  ([x y]

   (cond
     (nil? y) x

     (map? y) (reduce ++ x y)

     (map-entry? y)
     (let [{:keys [get set]} (lens (key y))]
       (if-let [v (get x)] (set x v) x))

     (every? map? [x y])
     (merge-with ++ x y)

     (every? cfn? [x y])
     (->cfn (comp y x))

     (vector? x)
     (++ (reduce ++ x) y)

     (vector? y)
     (reduce ++ x y)

     (cfn? y) (y x)

     (seq? x)
     (concat x (if (seq? y) y [y]))

     :else y))

  ([x y & xs]
   (reduce ++ (++ x y) xs)))

(comment

  (++ '() 1 2 3)
  (++ {}
      {:a '(1 2)}
      [{:a 3 :b {:c 1}} {:b {:d 2}}]
      {:b {:d (cfn inc)}}
      (++ {:a (cfn (flatten (repeat 3 _)))}
          {:a (cfn (do (println "got " _) (reverse _)))}))

  ((cfn (+ _ _)) 1))

(defrecord Component
  [tag style attrs childs injections])

(defn component? [x]
  (instance? Component x))

(comment

  (defn injection-pattern? [x]
    (and (keyword? x)
         (-> x name first #{"." "#"})))

  (defn parse-tag [x]
    (let [id? (some #{"#"} (seq (name x)))
          [tag & segs] (str/split (name x) #"[#\.]")
          [id & class] (if id? segs (cons nil segs))]
      {:tag (when-not (= "" tag) tag)
       :id id :class class}))

  (defn match? [pat comp]
    (let [{:keys [id class]} (parse-tag pat)]
      (or (and id (= id (get-in comp :attrs :id)))
          (some (set class) (get-in comp :attrs :class)))))

  (defn injections?
    [x]
    (and x
         (not (component? x)) ;; just to speed things up
         (or (and (map? x) (seq x) (every? injection-pattern? (keys x)))
             (and (vector? x) (every? injections? x)))))

  (defn prop?
    "x represent either a style or attrs"
    [x]
    (and x
         (not (component? x))
         (not (injections? x))
         (or (map? x)
             (and (vector? x)
                  (every? prop? x)))))

  (def component-key?
    #{:style :attrs :injections :tag :childs})

  (defn tag? [x]
    (and (keyword? x)
         (not (component-key? x))
         (merge {:tag :div} (parse-tag x))))

  (defn split-named-entries
    ([xs] (split-named-entries xs nil))
    ([xs ret]
     (if (component-key? (first xs))
       (split-named-entries
         (drop 2 xs)
         (assoc ret (first xs) (second xs)))
       [ret xs])))

  (defn seq-extract
    ([f xs]
     (println 'seq-extract xs)
     (seq-extract f xs [[] []]))
    ([f xs ret]
     (if (seq xs)
       (let [x1 (first xs) xs (next xs)]
         (seq-extract f xs
                      (if (f x1) [(conj (get ret 0) x1) (get ret 1)]
                                 [(get ret 0) (conj (get ret 1) x1)])))
       ret)))

  (defn build [x & xs]
    (let [[{:keys [tag id class]} xs]
          (if (tag? x) [(parse-tag x) xs] [{:tag :div} (cons x xs)])
          [named-entries xs] (split-named-entries xs)
          [[injections] xs] (seq-extract injections? xs)
          [[style attrs] childs] (seq-extract prop? xs)]
      (println named-entries injections style attrs)
      (map->Component
        (++ named-entries
            {:tag tag
             :injections injections
             :style style
             :attrs (++ {:id id :class class} attrs)
             :childs (reduce ++ () childs)}))))

  (def c build)
  (build :hey.ouep
         :style {:pouet :pouet}
         [{:bg :red} {:p 1}]
         [{:on-click (fn [])} {}])

  (defn $childs [f]
    {:childs (cfn (map f _))})

  (++ (c "iop") ($childs (partial str "poupou-")))

  (defn inject [injections c]
    (println "inject " injections c)
    (reduce
      (fn [c [pat trans]]
        (if (match? pat c) (++ c trans) c))
      c injections))

  (defn inject-childs [c]
    (update c :childs
            (partial map (partial inject (:injections c)))))

  (defn render [{:as c :keys [style attrs tag]}]
    (into [(keyword tag) (assoc attrs :style style)]
          (:childs (inject-childs c))))

  (render (build :hey.ouep
                 :style {:pouet :pouet}
                 [{:bg :red} {:p 1}]
                 [{:on-click (fn [])} {}]))

  (defn parse-pattern [x]
    (cond
      (vector? x)
      {:literal x
       :deep true
       :patterns (map parse-pattern x)}
      (u/word? x)
      (let [s (name x)
            id? (some #{"#"} (seq s))
            recursive? (= "*" (first s))
            s (if recursive? (apply str (next s)) s)
            [tag & segs] (str/split (name s) #"[#\.]")
            [id & class] (if id? segs (cons nil segs))]
        {:literal (keyword x)
         :tag (when-not (= "" tag) tag)
         :id id
         :class class
         :recursive? recursive?})))

  (parse-pattern :*.iop)
  (parse-pattern '[.aze :#op *.foo])

  (declare injection)

  (defrecord Injection [test content])

  (def injection0
    (Injection. (cfn true) nil))

  (defn injection [& [t c]]
    (++ injection0
        {:test (->cfn t) :content c}))

  (defn inject [{:keys [test content]} c]
    (if (test c) (++ c content) c))

  (defn injection->cfn [i]
    (cfn (inject i _)))

  (defn injection-test_class [x]
    (let [cs (set (if (sequential? x) x (list x)))]
      (cfn (some cs (get-in _ [:attrs :class])))))

  (defn injection-test_id [x]
    (let [cs (set (if (sequential? x) x (list x)))]
      (cfn (some cs (get-in _ [:attrs :id])))))

  (defn injection-test_deep [xs]
    (if-not (seq xs)
      (cfn true)
      (cfn (and ((injection-test (first xs)) _)
                (++ _ {:childs (cfn cs (map (cfn c (inject c (map injection-test_deep (next xs))))
                                            cs))})))))

  (defn injection_always []
    (injection (cfn true)))

  (defn injection-test_and [xs]
    (if (seq xs)
      (let [injs (map #(injection_constructor % (constantly nil)) xs)]
        (injection_pred->constructor
          #(every? identity (map () injs))))
      (injection_always)))

  (defn injection-test [pat]
    (cond
      (vector? pat) (injection-test_deep pat)
      (u/word? x)
      (let [s (name x)
            id? (some #{"#"} (seq s))
            recursive? (= "*" (first s))
            s (if recursive? (apply str (next s)) s)
            [tag & segs] (str/split (name s) #"[#\.]")
            [id & class] (if id? segs (cons nil segs))]
        (++ (when id) ())
        {:literal (keyword x)
         :tag (when-not (= "" tag) tag)
         :id id
         :class class
         :recursive? recursive?})
      ()))

  )

(defrecord Lens [get set])

(defn lens? [x] (instance? Lens x))

(defn key-lens [k]
  (Lens. #(get % k)
         #(assoc %1 k %2)))

(defn deep-lens [v]
  (Lens. #(get-in % k)
         #(assoc-in %1 k %2)))

(defn upd [x lens f]
  ((:set lens) x (f ((:get lens) x))))

(defn lens
  ([x]
   (cond
     (keyword? x) (key-lens x)
     (vector? x) (deep-lens x)
     :else (throw (js/Error. "not lensable"))))
  ([x y] (Lens. x y)))

(upd {:iop 1} (lens :iop) inc)

(upd {:a {:b 1}} (merge-with comp
                             (lens :a)
                             (lens :b)) inc)



