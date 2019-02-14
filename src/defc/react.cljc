(ns defc.react
  "`defc` applied to react"
  (:require [defc.core :as defc])
  #?(:cljs (:require-macros [defc.react :refer [defc]]))
  #?(:cljs (:require ["react" :as React] ["react-dom" :as Dom])))


(def create-element #?(:cljs (.-createElement React) :clj  vector))


(defc/make-wrapper create-element)


(defn render
  ([el node] (render el node nil))
  ([el node cb] #?(:cljs (.render Dom (<r el) node cb) :clj nil)))
