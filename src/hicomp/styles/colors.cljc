(ns hicomp.styles.colors
  (:require [thi.ng.math.core :as m]
            [thi.ng.color.core :as c]
            [thi.ng.color.presets :as cp]
            [thi.ng.color.gradients :as cg]))

#?(:clj (prefer-method print-method clojure.lang.IDeref java.util.Map))

(defn grey [n]
  (str "rgb("
       (apply str (interpose "," (repeat 3 (int (+ 75 (* 180 (/ n 22)))))))
       ")"))

(def greyscale (vec (next (reverse (map grey (range 23))))))

(defn greymap [prefix xs]
  (into {} (map-indexed
             (fn [i v]
               [(keyword (str (name prefix) "grey" i)) v])
             xs)))

(def lightgreys
  (greymap :light (take 7 greyscale)))

(def midgreys
  (->> greyscale (drop 7) (take 7) (greymap :mid)))

(def darkgreys
  (greymap :dark (drop 14 greyscale)))

(def greys
  (merge
    lightgreys
    midgreys
    darkgreys
    (greymap "" greyscale)))


(def table

  (merge

    {:left :my-green
     :right :my-blue
     :error :tomato-light

     :azure ":#00cccc"
     :spring-green "#00EC76"

     :tomato-light "#FF7259"
     :my-blue "#82ccdd"
     :my-green "#b8e994"
     :white "white"
     :grey :midgrey0
     :lightgrey :lightgrey0}

    greys))

(defn get-color [k]
  (when-let [v (get table k)]
    (if (keyword? v)
      (get-color v)
      v)))

@(c/as-css (c/rgba 1 0 0 0.5))

(m/mix (cp/preset-rgb :green)
       (cp/preset-rgb :white)
       0.5)

(defn preset-hsv [id] (some->> id cp/colors c/int24 c/as-hsva))

(defn hsla [color]
  (c/as-hsla
    (or (and (satisfies? c/IColorComponents color) color)
        (preset-hsv color)
        (c/css (name color)))))

(defn color [x]
  #_(println "color " x " " (or (get-color x) "css"))
  (let [k (if (keyword? x) x (keyword (name x)))]
    (or (get-color k)
        (-> (hsla k) c/as-int24 c/as-css deref))))

(defn shades [col n]
  (let [col (hsla col)]
    (mapv (fn [m] (color (assoc col :l (/ m (dec n)))))
          (range n))))


(-> (hsla :red) c/as-int24 c/as-css deref)