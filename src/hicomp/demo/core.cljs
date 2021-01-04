(ns hicomp.demo.core
  (:require [reagent.dom :as rd]
            [stylefy.core :as stylefy]
            [hicomp.demo.article :as article]))

(defn ^:export render []
  (rd/render [article/component]
             (js/document.getElementById "app")))

(defn ^:export init []
  (stylefy/init)
  (render))