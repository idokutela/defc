(ns defc.core
  (:require [clojure.string :as string])
  #?(:cljs (:require-macros [defc.core :refer [make-wrapper make-<r make-defc]])))


#? (:clj (defn namespaced-symbol [sym]
           (symbol (name (ns-name *ns*)) (if (symbol? sym) (name sym) sym))))


(defn flatten-children
  "Takes a seq that contains hiccup `children`, and flattens it."
  [children]
  (mapcat #(if (seq? %) (flatten-children %) (list %)) (filter some? children)))


(defn process-first-when
  ([coll pred f] (process-first-when coll pred f nil))
  ([coll pred f default]
   (if (and (not-empty coll) (-> coll first pred))
     [(f (first coll)) (rest coll)]
     [default coll])))


(defn camel<-spit [s]
  (let [[prefix & parts] (string/split (name s) #"-")]
    (->> parts (map string/capitalize) (cons prefix) string/join)))


(defn props<-args [args]
  (let [args (or args ())]
    #?(:clj {:cljsArgs args} :cljs #js {:cljsArgs args})))


(defn args<-props [props]
  #?(:clj (when-first [props props] (when (map? props) (:cljs-args props)))
     :cljs (when-first [props props] (when (object? props) (.-cljsArgs props)))))


(defn props<-map [m]
  #?(:clj m
     :cljs (clj->js m :keyword-fn camel<-spit)))


(defn wrap-children [args]
  (when (not-empty args) #?(:clj (vec args) :cljs (apply array args))))


(defn compile-hiccup
  [[el & args] {:keys [<r props<-map create-element]}]
  (let [[el args] (if (= :#js el) args [(name el) args])
        [props children] (process-first-when args map? props<-map)
        children (->> children flatten-children (map <r) (filter some?) wrap-children)]
    (create-element el props children)))


(defmacro make-<r
  [create-element & {:keys [<r props<- props<-map str<-kw]
                     :or {<r '<r props<- `props<-args props<-map `props<-map str<-kw `camel<-spit}}]
  `(defn ~<r [v#]
     (if-not (vector? v#) v#
       (let [[f# ~'& args#] v#]
         (cond
           (fn? f#) (~create-element f# (~props<- args#))
           (keyword? f#)
           (compile-hiccup
            v# {:<r ~<r :str<-kw ~str<-kw :props<-map ~props<-map :create-element ~create-element})
           :else (throw (ex-info "Invalid hiccup" {:hiccup v#})))))))


(defmacro make-defc [_ & {:keys [defc args<- <r] :or {defc 'defc args<- `args<-props <r '<r}}]
  (let [<r (namespaced-symbol <r)]
    `(defmacro ~defc [f# & fdecl#]
       (let [[pre# fdecl#] (process-first-when fdecl# string? (partial vector f#) [f#])
             [pre# fdecl#] (process-first-when fdecl# map? (partial conj pre#) pre#)
             args# (gensym "args")
             apply-f# (list `apply (cons `fn (cons f# fdecl#)) args#)
             fdecl# (list ['& args#]
                          (list `if-some [args# (list (quote ~args<-) args#)]
                                (list (quote ~<r) apply-f#) apply-f#))]
         (cons `defn (concat pre# fdecl#))))))


(defmacro make-wrapper [& args]
  `(do (make-<r ~@args) (make-defc ~@args)))
