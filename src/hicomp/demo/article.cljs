(ns hicomp.demo.article
  (:require
    [reagent.core :as rea]
    [hicomp.core :refer [c div span #_defc]]
    [hicomp.styles.mixins :as s]
    [hicomp.styles.colors :as c]
    [hicomp.demo.utils :refer-macros [card card* expr code]]
    [markdown-to-hiccup.core :as md :refer [md->hiccup]]))

(defn icon-button
  [class & [styles attrs]]
  (c [{:color :midgrey0
       :text :md :p 0.5
       :hover {:color :tomato}}
      styles]
     [{:key class} attrs]
     (c :i {}
        {:class (str "fa fa-" (name class))})))

(defn markdown [x]
  (->> x
       md->hiccup
       md/component
       (c :.markdown-body)))

(defn section
  [{:keys [styles attrs title content]}]
  (let [state (rea/atom {:open true})
        if-open (fn [a b] (if (:open @state) a b))
        border {:width 3 :color :lightgrey0}]
    (fn []
      (c :.section
         [{:m {:top 2} :border {:top border :left border}} styles]
         attrs
         (c :.title
            {:flex [:inline :center {:items :baseline}]
             :hover {:.collapse-button {:display :inline}}}
            {:on-click #(swap! state update :open not)}
            title
            (icon-button (if-open :minus-square :plus-square)
                         {:color :lightgrey2}))
         (c :.content
            {:p {:left 2} :display (if-open :block :none)}
            (map (fn [x] (if (string? x) (markdown x) x)) content))))))

(letfn [(builder [size]
          (fn [title & content]
            [section {:title title :content content
                      :styles {:.title {:text [size :bold] :p 2}}}]))]
  (def S1 (builder :3xl))
  (def S2 (builder :2xl))
  (def S3 (builder :xl))
  (def S4 (builder :lg)))

(defn component []

  (c {:m 3 :p 3}

     (card (c {:flex [:center :wrap]}
              (map (fn [col] (c {:p 2 :display :inline-block
                                 :flex-basis "10%" :bg {:color col}}))
                   (c/shades :gray 30))))

     [S1 :Hicomp

      "Hicomp is a small library that lets you create and compose simple static hiccup components."

      "We can create a component like this:"

      (card* "Hello")

      "It can take styles:"

      (card* {:color :grey} "Hello")

      "Attributes:"

      (card* {:color :grey}
             {:on-click (fn [_] (js/alert "hello!"))}
             "Click me")

      "Nothing fancy so far, but wait a minute, how about this ?"

      (card* :button
             {:rounded 2 :p 1
              :color :white
              :bg {:color :lightgrey}
              :hover {:bg {:color :tomato}}}
             "Hover Me")

      "Hicomp uses stylefy under the hood, and is letting you use pseudo classes and subselectors"

      (card* {:color :grey
              :.sub {:color :tomato}}
             "I can inject styles to my sub components!"
             (map (fn [t] [:div.sub t]) (range 3)))

      "if you try the same with raw hiccup you will have a nasty warning from react "

      (card [:div (map (fn [t] [:div.sub t]) (range 3))])

      "Even more handy hicomp will flat any nested seq of components without complaning"

      (card* (interpose "|" (map (fn [x] (repeat x (c :span {:m 1} x))) (range 1 5))))

      "The same is true for styles and attrs"

      (card (let [ss {:p 1 :bg {:color :grey} :rounded 2}]
              (c [ss {:m :auto :flex [:inline :center] :bg {:color :salmon} :color :white}]
                 "Content")))

      "You can supply nested sequentials values containing styles declarations, all will be be deep-merged in order"

      [S2 :Styles

       "You may have noticed that styles we are using in our components are not regular css ones, some are nested, some do not exists. Under the hood hicomp uses a collection of clojure functions to produce styles, along with stylefy that do the heavy lifting"

       "Those functions are locatted in the `hicomp.styles.mixins` namespace, in this section we will introduce some of them."

       [S3 "size"
        #_(code (defn show-size [size]
                (c :.container
                   {:bg {:color :midgrey0}
                    :rounded 1
                    :size [:full "100px"]
                    :p 1}
                   (c {:size size :flex :center :bg {:color :lightgrey} :color :white :rounded 1}
                      (str size)))))

        "numbers are interpreted as percentages"
        (expr (s/size 50 50))
        #_(show-size [50 50])
        "Arity one is handy if x size and y size are equals"
        (expr (s/size 50))
        "Any hiccup recognized value is valid"
        (expr (s/size :3rem))
        "There is some predefined aliases"
        (expr (s/size :full :half))
        (expr (s/size :third))
        "You can specify min and max values to"
        (expr (s/size {:min "100px" :max :half} {:min "100px" :val :full}))
        "You can set width and height independently too"
        (expr (s/width :full))
        (expr (s/height "2rem"))
        (expr (s/width {:min "50px" :max 50}))]

       [S3 :Colors
        (expr (s/color :tomato))
        (expr (s/color :light-skyblue))]

       ]
      ]
     ))
