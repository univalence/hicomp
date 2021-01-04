(ns hicomp.core
  (:require [stylefy.core :as stylefy]
            [hicomp.utils :as u]
            [hicomp.styles.mixins :as s]
            [clojure.string :as str])
  #_(:cljs (:require-macros [hicomp.core :refer []])))

(u/marked-fn cfn
             "a marked function to denote component updates")

;; impl
;; -----------------------------------------------------------

;; km is short for keyword-map
;; this will serve to treat html-attributes and style-mixins
;; the intent is to ease a bit creation and composition of this kind of maps
;; with this you should be able to avoid writing 'merge 'assoc 'assoc-in etc...
;; I tend to think that those kinds of maps representing 95% of our map usage, they deserves concise api

(defn km+
  "deeply-merge 2 kms"
  [x y]
  (cond
    (nil? x) y
    (nil? y) x

    (every? map? [x y])
    (merge-with km+ x y)

    (cfn? y) (y x)

    :else y))

(defn km
  "build a km
   can be given a map
   or a seq of things that km accepts :)"
  ([x]
   (cond (map? x) x
         (sequential? x)
         (reduce km+ {} (map km x))))
  ([x & xs]
   (km (cons x xs))))

(defn ikm?
  "am I a valid km argument?"
  [x]
  (or (map? x) (nil? x)
      (and (sequential? x)
           (every? ikm? x))))

;; preds

(defn hiccup? [x]
  (and (vector? x)
       (or (fn? (first x))
           (keyword? (first x)))))

(defn content? [x]
  (or (string? x) (keyword? x)
      (symbol? x) (number? x)))

(defn childs? [x]
  (or (nil? x) (hiccup? x)
      (cond (sequential? x) (every? childs? x)
            (content? x) x)))



;; conform

(defn hiccup_tag [x]
  (cond (nil? x) :div
        (keyword? x)
        (if (-> x name first #{"." "#"})
          (keyword (str "div" (name x)))
          x)
        :else x))

(defn hiccup_format [[tag & body]]
  (into [(hiccup_tag tag)] body))

(defn childs [x]
  (cond (nil? x) []
        (hiccup? x) [(hiccup_format x)]
        (content? x) [x]
        (sequential? x) (mapcat childs x)
        :else []))



;; style
;; --------------------------------------------------------------

;; the following is built on top of https://github.com/Jarzka/stylefy
;; and parkaviz.client.styles.mixins
;; the intent is to provide a concise API to declare components with styles
;; It will support regular hiccup inline-styles along with css nested selectors, pseudo classes and modes
;; the whole in a unified and expressive API powered by a css-mixin library

(def style_modes
  #{:active :blank :checked :current :default :link
    :disabled :enabled :focus :hover :valid :visited})

(def style_pseudo-elements
  #{"after" "backdrop" "before" "cue" "first-letter" "first-line" "part"
    "grammar-error" "marker" "placeholder" "selection" "spelling-error"})

(defn style_key [& xs]
  (keyword (str/join "-" (map name xs))))

(declare style_flat)

(defn style_prefix [p m]
  (u/map-keys (partial style_key p)
              (style_flat m)))

(defn style_flat
  "styles can be defined with nested maps
   e.g {:font {:style _ :weight _}}
   this function will turn this to
   {:font-style _ :font-weight _}"
  [x]
  (reduce
    (fn [a [k v]]
      (merge
        a (if (map? v)
            (style_prefix k v)
            {k v})))
    {} x))

(defn style_modal-selector [x]
  (keyword (str "&" x)))

(defn style_pseudo-selector [x]
  (and (keyword? x)
       (namespace x)
       (style_pseudo-elements (name x))
       (str "&::" (name x))))

(defn style_selector? [x]
  (or (string? x)
      (-> x name first #{"." "#"})))

(defn style_expand-mixins
  [x]
  (->> (km x)
       (map (fn [[k v]]
              (if-let [f (s/mixin k)]
                (if (vector? v) (apply f v) (f v))
                {k v})))
       km style_flat))

(defn style_compile [x]
  (let [f (partial u/map-vals style_compile)]
    (-> (update x :self style_expand-mixins)
        (update :pseudo f)
        (update :modes f)
        (update :sub f))))

(declare style_mk)

(defn style_parse [x]
  (reduce
    (fn [a [k v]]
      (cond
        (style_selector? k) (assoc-in a [:sub k] (style_mk v))
        (style_modes k) (assoc-in a [:modes (style_modal-selector k)] (style_mk v))
        (style_pseudo-selector k) (assoc-in a [:pseudo (style_pseudo-selector k)] (style_mk v))
        :else (assoc-in a [:self k] v)))
    {} x))

(defn style_mk [x]
  (cond
    (nil? x) {}
    (map? x) (-> x style_parse style_compile)
    (sequential? x) (km (map style_mk x))))

(defn style_emit
  [{:as parsed-style
    :keys [sub self modes pseudo]}]
  (into [self]
        (map (fn [[k v]] (into [k] (style_emit v)))
             (concat modes pseudo sub))))

(defn style_usable [style]
  (let [[style & manual] (-> style style_mk style_emit)]
    (assoc style :stylefy.core/manual (vec manual))))

;; api
;; --------------------------------------------------------------

(defn props
  "build html attributes, use stylefy instead of inline-styles
   attrs and style are given in 'km form, allowing more concise composition
   style will be compiled using our previously described strategies"
  ([attrs] (props {} attrs))
  ([style attrs]
   (assert (ikm? style) (str "invalid style:\n" style))
   (assert (ikm? attrs) (str "invalid attrs:\n" attrs))
   (stylefy/use-style (style_usable style)
                      (km attrs))))

(defn style [& xs]
  (props xs {}))

(defn raw-style [& xs]
  (style_usable xs))

(defn component_parse-head [[x & xs]]
  (let [[el [x & xs]] (if (keyword? x) [x xs] [nil (cons x xs)])
        [style [x & xs]] (if (ikm? x) [x xs] [{} (cons x xs)])
        [attrs tail] (if (ikm? x) [x xs] [{} (cons x xs)])]
    {:tag (hiccup_tag el)
     :style style
     :attrs attrs
     :tail tail}))

(defn component_parse [xs]
  (let [{:as parsed :keys [tag style attrs tail]} (component_parse-head xs)]
    (assert (childs? tail)
            (str "invalid childs " style attrs tail))
    (-> parsed
        (dissoc :tail)
        (assoc :childs (childs tail)))))

(defn component_render
  [{:keys [tag style attrs childs]}]
  (into [tag (props style attrs)] childs))

(defn component
  "return a hiccup component
   can optionaly take an element tag as first argument (e.g :div, :span.myspan...)
   some styles (with support for modes, nested selectors and pseudo-classes)
   some attrs (one or many maps)
   childs (one or many hiccup-elements or string or sequence of childs)
   for details see below mini tutorial"
  [& xs]
  (-> xs component_parse component_render)
  #_(let [{:keys [tag style attrs childs]}
          (component_parse xs)]
      (into [tag (props style attrs)] childs)))

(defn mcomponent [& xs]
  (component_parse xs))

(defn mcomponent+ [& xs]
  (reduce merge-with))

;; shorthands

(def c component)
(def m component-map)
(def div (partial c :div))
(def span (partial c :span))

(comment

  (component {:color :right
              :display :inline} ;;styles
             {:id "IO"} ;;attrs
             [:span "pouet"] ;;child
             )

  ;; the most basic form
  (component {:color :right} ;;styles
             {:id "IO"} ;;attrs
             [:span "pouet"] ;;child
             )

  ;; we can specify the element html tag
  (component :span ;; the element tag
             {:color "blue"} ;;styles
             {:id "IO"} ;;attrs
             ;; several childs
             [:span "pouet"]
             [:span "ciboulette"])

  ;; component do not have to take styles and attrs
  (component [:div "iop"])

  ;; it can take only styles
  (component {:color :error} [:div "arg"])

  ;; if component has no styles but have attrs i'm afraid you have to write this
  (component {} {:id "IO"} [:div "arg"])
  ;; but you can always use raw hiccup form in this case
  [:div {:id "IO"} [:div "arg"]]

  ;; there is a shortcut for 'component
  (c {} {:id "IO"} [:div "arg"])


  ;; you can omit div in hiccup tag literals
  (c :#me.iop "iop") ;; equiv (c :div#me.iop "iop")

  ;; arguments can be sequences
  (c :span ;; the element tag
     [{:color "blue"} {:background-color "pink"}] ;;styles
     [{:id "IO"} {:on-click (fn [])}] ;;attrs
     ;; several childs
     [:span "pouet"]
     (map (partial vector :div) '[am stram gram]) ;; childs can contain sequence too
     [:span "ciboulette"])

  ;; inline styles will be handled by stylefy
  ;; childs by being systematically flattened and realised remove the reagent annoyance with lazy-seqs
  ;; styles, attrs and childs being recursivelly flattened, it permits easier composition

  (let [mixin1 {:flex [:center :inline]}
        mixins [{:width "100%"} {:height "20px"}]]
    #_(styles-and-classes [classes mixin1 mixins])
    (c [mixin1 mixins] ;; nested styles and classes
       {:on-click (fn [])}
       [:span "io"]
       ;; a sequence of sequence of elements
       (map (fn [n] (map (partial vector :div) (range n))) [3 4 5])))

  )

(def component-spec0
  {:tag :div :style {} :attrs {} :childs ()})

(defn c+ [& xs]
  (component_render
    (reduce km+ component-spec0 xs)))

#_(component_render (style_usable {:focus {:w 1}}))

(comment
  (defmacro fnc [x & xs]
    (let [[name [pattern & xs]]
          (if (symbol? x) [x xs] [(gensym "hicomp_") (cons x xs)])]
      `(let [{tag# :tag style# :style attrs# :attrs
              tail# :tail} (component_parse-head xs)]
         (fn [& xs#]
           (let [{style# :style attrs# :attrs
                  childs# :childs} (hicomp.core/component_parse xs#)
                 ~childs-pattern childs#]
             (c [~base-style style#]
                [~base-attrs {:component-name '~name} attrs#]
                ~body))))))

  (defmacro defc [name & xs]
    `(def ~name (fnc ~name ~@xs)))

  (defn merge_compiled
    [[tag1 attrs1 & body1]
     [tag2 attrs2 & body2]]
    (let [tag (if (= tag2 :div) tag1 tag2)
          attrs (km attrs1 attrs2 (update attrs1 ::stylefy/manual into (::stylefy/manual attrs2)))
          body (into body1 body2)]
      (into [tag attrs] body)))

  #_(merge_compiled
      (c {:class ""}))

  (comment
    (defc test-c
          {:p 1}
          {:on-click (fn [_] (u/pp "iop"))})

    (test-c "iop")

    (let [test-c2 (fnc
                    {:p 1}
                    {:on-click (fn [_] (u/pp "iop"))})])))

