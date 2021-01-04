(ns hicomp.styles.mixins
  (:refer-clojure :exclude [float floats])
  (:require [hicomp.utils.css :as css]
            [hicomp.utils :as u]
            [hicomp.styles.colors :as sc]
            [hicomp.styles.dimensions :as dims]))

;; colors
;; ----------------------------------------------------------------

(defn color [x]
  {:color (sc/color x)})

(defn bg-color [x]
  {:background-color (sc/color x)})

;; display
;; ----------------------------------------------------------------

(def displays
  #{:block
    :inline-block
    :inline
    :flex
    :inline-flex
    :table
    :table-row
    :table-cell
    :none})

(defn display [x]
  {:display (get displays x)})

;; size
;; ----------------------------------------------------------------

(defn width [x]
  (cond
    (map-entry? x)
    (let [k (key x) v (val x)]
      (case k
        :min {:min-width (dims/size v)}
        :max {:max-width (dims/size v)}
        :val (width v)))
    (map? x)
    (reduce merge {} (map width x))
    :else
    {:width (dims/size x)}))

(defn height [x]
  (cond
    (map-entry? x)
    (let [k (key x) v (val x)]
      (condp = k
        :min {:min-height (dims/size v)}
        :max {:max-height (dims/size v)}
        :val (height v)))
    (map? x)
    (reduce merge {} (map height x))
    :else
    {:height (dims/size x) }))

(defn size
  ([x] (size x x))
  ([w h] (merge (width w) (height h))))

;; flexbox
;; ----------------------------------------------------------------

(def flex-flags
  {:center {:justify-content :center :align-items :center}
   :wrap {:flex-wrap :wrap}
   :nowrap {:flex-wrap :nowrap}
   :column {:flex-direction :column}
   :row {:flex-direction :row}
   :grow {:flex-grow 1}
   :nogrow {:flex-grow 0}
   :shrink {:flex-shrink 1}
   :noshrink {:flex-shrink 0}
   :inline {:display :inline-flex}})

(defn- flex-return [& xs]
  ;; inline-flex has precedence over flex
  (let [inline? (some #(= :inline-flex (:display %)) xs)]
    (assoc (reduce merge xs)
      :display
      (if inline? :inline-flex :flex))))

(defn flex

  ([]
   {:display :flex})

  ([x]
   (cond
     (keyword? x)
     (flex {x true})
     (set? x)
     (flex (zipmap x (repeat true)))
     (map? x)
     (let [{:keys [dir items content justify
                   grow shrink self basis order]} x]
       (flex-return
         (u/remove-nil-vals
           {:flex-direction dir
            :align-self self
            :align-items items
            :align-content content
            :justify-content justify
            :flex-grow grow
            :flex-shrink shrink
            :flex-basis (when basis (dims/size basis))
            :order order})
         (->> (u/get-flags x)
              (select-keys flex-flags)
              (vals) (apply merge))))))
  ([x & xs]
   (apply flex-return (flex x) (map flex xs))))

(defn flexi
  [grow shrink basis]
  (flex
    {:grow grow
     :shrink shrink
     :basis basis}))

;; spacing
;; ----------------------------------------------------------------

(def directions [:bottom :top :left :right])

(dims/define-space-mixin margin margin m)
(dims/define-space-mixin padding padding p)

;; border
;; ----------------------------------------------------------------

(defn border-props [x]
  (when (seq x)
    (-> {:width 2
         :style :solid
         :color :lightgrey}
        (merge x)
        (update :width css/size)
        (update :style #{:solid :dashed :dotted :double :none})
        (update :color sc/color))))

(defn prefix-keys [prefix m]
  (u/map-keys
    (fn [k] (keyword (str (name prefix) (name k))))
    m))

(defn border
  ([{:as opts
     :keys [dir]}]
   (let [prefix
         (if dir
           (str "border-" (name dir) "-")
           "border-")]
     (apply merge
            (->> (select-keys opts [:width :style :color])
                 (border-props) (prefix-keys prefix))
            (->> (select-keys opts [:x :y :left :top :bottom :right])
                 (mapv (fn [[dir opts]]
                         (case dir
                           :x (border {:top opts :bottom opts})
                           :y (border {:right opts :left opts})
                           (border (assoc opts :dir dir)))))))))
  ([width color]
   (border
     {:color color :width width})))

(defn border-radius-value [x]
  (cond
    (integer? x) (str (* x 0.25) "rem")
    (= :full x) "9999px"
    :else (css/size x)))

(defn rounded
  "border radius mixin"
  ([x]
   (cond
     (map-entry? x)
     (let [k (key x) v (val x)]
       (condp = k
         :top (rounded v v nil nil)
         :bottom (rounded nil nil v v)
         :right (rounded nil v v nil)
         :left (rounded v nil nil v)))
     (map? x)
     (reduce rounded {} x)
     :else
     (rounded x x x x)))
  ([tl tr br bl]
   (u/remove-nil-vals
     {:border-top-left-radius (border-radius-value tl)
      :border-top-right-radius (border-radius-value tr)
      :border-bottom-left-radius (border-radius-value bl)
      :border-bottom-right-radius (border-radius-value br)})))

(def font-shadows
  {:normal "0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)"
   :sm "0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)"
   :md "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)"
   :lg "0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)"
   :xl "0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)"
   :xxl "0 25px 50px -12px rgba(0, 0, 0, 0.25)"
   :inner "inset 0 2px 4px 0 rgba(0, 0, 0, 0.06)"
   :outline "0 0 0 3px rgba(66, 153, 225, 0.5)"
   :none "none"})

(defn shadow
  ([] (shadow :normal))
  ([x] {:box-shadow (get font-shadows x)}))

;; text
;; ----------------------------------------------------------------

(def font-families
  {:sans "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
   :serif "Georgia, Cambria, \"Times New Roman\", Times, serif;"
   :mono "Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"
   :fira "Fira Code"})

(defn font-family [x]
  {:font-family (get font-families x)})

(def font-sizes
  {:xs ".75rem"
   :sm ".875rem"
   :base "1rem"
   :md "1rem"
   :lg "1.125rem"
   :xl "1.25rem"
   :xxl "1.5rem"
   :2xl "1.5rem"
   :3xl "1.875rem"
   :xxxl "1.875rem"
   :4xl "2.25rem"
   :5xl "3rem"
   :6xl "4rem"})

(defn font-size [x]
  (cond
    (nil? x) {}
    (keyword? x) {:font-size (get font-sizes x)}
    :else {:font-size (css/size x)}))

(def font-weights
  {:thinest 100
   :thin 200
   :light 300
   :normal 400
   :medium 500
   :semibold 600
   :bold 700
   :bolder 800
   :boldest 900})

(defn font-weight [x]
  {:font-weight (get font-weights x x)})

(def antialiased
  {"-webkit-font-smoothing" :antialiased
   "-moz-osx-font-smoothing" :grayscale})

(def subpixel-antialiased
  {"-webkit-font-smoothing" :auto
   "-moz-osx-font-smoothing" :auto})

(def italic
  {:font-style :italic})

(def not-italic
  {:font-style :normal})

(def text-transforms
  {:uppercase :uppercase
   :lowercase :lowercase
   :capitalize :capitalize
   :normal-case :none})

(defn text-transform [x]
  {:text-transform
   (get text-transforms x)})

(def vertical-aligns
  #{:baseline
    :top
    :middle
    :bottom
    :text-top
    :text-bottom})

(def text-aligns
  #{:left
    :center
    :right
    :justify})

(defn text-align [x]
  (cond
    (vertical-aligns x) {:vertical-align x}
    (text-aligns x) {:text-align x}))

(def text-trackings
  {:tighter "-0.05em"
   :tight "-0.025em"
   :normal "0"
   :wide "0.025em"
   :wider "0.05em"
   :widest "0.1em"})

(defn text-tracking [x]
  {:letter-spacing
   (get text-trackings x x)})

(def text-leadings
  {:none 1
   :tight 1.25
   :snug 1.375
   :normal 1.5
   :relaxed 1.625
   :loose 2})

(defn text-leading [x]
  {:line-height
   (get text-leadings x x)})

(def text-decorations
  #{:underline :line-throug :none})

(defn text-decoration [x]
  {:text-decoration (get text-decorations x)})

(def whitespaces
  #{:normal :nowrap :pre :pre-line :pre-wrap})

(defn whitespace [x]
  {:white-space (whitespaces x)})

(def text-break
  {:normal {:word-break :normal :overflow-wrap :normal}
   :words {:overflow-wrap :break-word}
   :all {:word-break :break-all}
   :truncate {:overflow :hidden
              :text-overflow :ellipsis
              :white-space :nowrap}})

(defn- text_find-direct-value
  [text-spec x]
  (let [valset (cond (set? x) x
                     (map? x) (set (keys x)))]
    (->> (keys text-spec)
         (filter valset)
         (first))))

(defn text
  ([x]
   (cond
     (keyword? x) (text {x true})
     (set? x) (text (zipmap x (repeat true)))
     (map? x)
     (let [{:keys [weight size style family align transform
                   italic tracking leading decoration break]} x
           family (or family (text_find-direct-value x font-families))
           weight (or weight (text_find-direct-value x font-weights))
           size (or size (text_find-direct-value x font-sizes))
           transform (or transform (text_find-direct-value x text-transforms))
           align (or align (text_find-direct-value x (into vertical-aligns text-aligns)))
           style (or style (and italic :italic))]
       (u/remove-nil-vals
         (merge
           (font-size size)
           (font-weight weight)
           (font-family family)
           (text-transform transform)
           (text-align align)
           (text-tracking tracking)
           (text-leading leading)
           (text-decoration decoration)
           (text-break break)
           (whitespace (:whitespace x))
           {:font-style style}
           (when (:antialiased x) antialiased)
           (when (:subpixel-antialiased x) subpixel-antialiased))))))
  ([x & xs]
   (apply merge (map text (cons x xs)))))

(comment
  (text :bold :xl :sans :center :bottom
        {:tracking :tight}))

;; position
;; ----------------------------------------------------------------

(def positions
  #{:static :fixed :absolute :relative :sticky})

(defn- placement-spec [x]
  (u/map-vals
    #(if (= :auto %) % (css/size %))
    x))

(defn position
  ([pos] {:position (get positions pos)})
  ([pos placement]
   (merge (position pos)
          (cond

            (vector? placement)
            (let [[xt yr & [b l]] placement]
              (if b {:top xt :right yr :bottom b :left l}
                    {:x xt :y yr}))

            (map? placement)
            (merge (placement-spec (select-keys placement [:top :right :bottom :left]))
                   (when-let [xp (:x placement)] (placement-spec {:right xp :left xp}))
                   (when-let [yp (:y placement)] (placement-spec {:top yp :bottom yp})))

            :else
            (placement-spec {:x placement :y placement})))))

;; background
;; ----------------------------------------------------------------

(def background-attachments
  #{:fixed
    :local
    :scroll})

(defn bg-attachment [x]
  {:background-attachment (background-attachments x)})

(def background-positions
  {:bottom "bottom"
   :center "center"
   :left "left"
   :left-bottom "left bottom"
   :left-top "left top"
   :right "right"
   :right-bottom "right bottom"
   :right-top "right top"
   :top "top"})

(defn bg-position [x]
  {:background-position (background-positions x)})

(def background-repeats
  #{:repeat
    :no-repeat
    :repeat-x
    :repeat-y
    :round
    :space})

(defn bg-repeat [x]
  {:background-repeat (background-repeats x)})

(def background-sizes
  #{:cover :contains :auto})

(defn bg-size [x]
  {:background-size (background-sizes x)})

(defn bg
  ([x]
   (u/remove-nil-vals
     (cond
       (keyword? x)
       (merge (bg-attachment x)
              (bg-position x)
              (bg-repeat x)
              (bg-size x))
       (map? x)
       (let [{:keys [repeat attachment size color position]} x]
         (merge (bg-repeat repeat)
                (bg-attachment attachment)
                (bg-position position)
                (bg-size size)
                (bg-color (sc/color color))
                (apply bg (u/get-flags x)))))))
  ([x & xs]
   (reduce merge (bg x) (map bg xs))))

;; misc
;; ----------------------------------------------------------------

(def centered-text
  {:text-align :center})

(def visible
  {:visibility :visible})

(def invisible
  {:visibility :hidden})

(def overflow
  {:auto {:overflow "auto"}
   :hidden {:overflow "hidden"}
   :visible {:overflow "visible"}
   :scroll {:overflow "scroll"}
   :x-auto {:overflow-x "auto"}
   :y-auto {:overflow-y "auto"}
   :x-hidden {:overflow-x "hidden"}
   :y-hidden {:overflow-y "hidden"}
   :x-visible {:overflow-x "visible"}
   :y-visible {:overflow-y "visible"}
   :x-scroll {:overflow-x "scroll"}
   :y-scroll {:overflow-y "scroll"}
   :scroll-touch {"-webkit-overflow-scrolling" :touch}
   :scroll-auto {"-webkit-overflow-scrolling: " :auto}})

(def object-positions
  {:bottom "bottom"
   :center "center"
   :left "left"
   :left-bottom "left bottom"
   :left-top "left top"
   :right "right"
   :right-bottom "right bottom"
   :right-top "right top"
   :top "top"})

(defn object-position [x]
  {:object-position (object-positions x)})

(def object-fits
  #{:contain :cover :fill :none :scale-down})

(defn object-fit [x]
  {:object-fit (get object-fits x)})

(defn obj
  ([x]
   (u/remove-nil-vals
     (cond (keyword? x)
           (merge (object-fit x)
                  (object-position x))
           (map? x)
           (let [{:keys [fit position]} x
                 flags (map key (filter (comp true? val) x))]
             (merge (object-fit fit)
                    (object-position position)
                    (apply obj flags)))))))

(def floats
  #{:right
    :left
    :none})

(defn float [x]
  {:float (floats x)})

(def clearfix
  {:stylefy.core/manual
   [[:&:after {:content ""
               :display :table
               :clear :both}]]})

(defn placeholder-color [x]
  {:stylefy.core/manual
   [[:&:placeholder {:color (sc/color x)}]]})

(defn resize [x]
  {:resize (#{:none :both :vertical :horizontal} x)})

(defn cursor [x]
  {:cursor (#{:pointer :auto :default :wait :text :move :not-allowed} x)})

;; table

(def table
  {:bg bg
   :text text
   :size size
   :flex flex
   :flexi flexi
   :shadow shadow
   :rounded rounded
   :border border
   :color color :col color
   :width width :w width
   :height height :h height
   :margin margin :m margin
   :padding padding :p padding
   :position position :pos position})

(defn mixin [k]
  (get table k))

(comment
  (flex :inline :center))