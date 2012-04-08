(ns datacontext.core
  (:require [datacontext.context :as context]
            [clojure.tools.logging :as logging]))

(defn def-context
  [key provide recover & [ops]]
  (let [provide (load-string (str "#'" provide))
        recover (if recover (load-string (str "#'" recover)))]
    (logging/debug (str "binding context [key:" key
                        " provide:" provide " recover:" recover " ops:" ops "]"))
    (context/check-context-fns provide recover)
    (let [use-argname (context/use-argname? provide)
        provide-remove-arg-count (if use-argname 2 1)]
      (context/bind-context key
                            {:key (name key)
                             :use-argname use-argname
                             :arglist (context/arg-list provide provide-remove-arg-count)
                             :provide (var-get provide)
                             :recover (if recover (var-get recover))
                             :ops ops}))))

(defn set-value!
  [key value]
  (context/change-value key value) nil)

(defn default-name-strategy [fn-name]
  (cond
   (.endsWith fn-name "0") (.substring fn-name 0 (- (count fn-name) 1))
   (.startsWith fn-name "inner-") (.substring fn-name (count "inner-"))
   :else (str fn-name "1")))

(defn wrap-pure-ns
  [& {:keys [ns to-ns name-strategy]
      :or {name-strategy default-name-strategy}}]
  (let [pure-ns (or (and  ns (the-ns (symbol ns))) *ns*)
        wraped-ns (or (and to-ns (create-ns (symbol to-ns))) pure-ns)
        to-be-wraped (->> (ns-publics pure-ns)
                (filter (fn [[sym var]]
                         (and (var? var)
                              (fn? (var-get var))
                              (:wrapcontext (meta var))))))]
    (doseq [[sym var] to-be-wraped]
      (let [f (context/wrap-pure-function var )
            fn-name (or ((meta var) :fn-name) (name-strategy (name sym)))
            fn-sym (with-meta (symbol fn-name) (context/wraped-meta var))]
        (intern wraped-ns fn-sym f)))))
