(ns datacontext.context
  (:require [clojure.tools.logging :as logging]))

(declare ^{:dynamic true :private true} *changed-values*)
(def ^:private contexts (atom {}))

(defonce key-arg-name "arg-name")
(defonce key-wrapcontext :wrapcontext)
(def err-provide-isfn "provide must be a function")
(def err-provide-overload "provide function must not has over-load arg lists")
(def err-provide-arg "provide function must has not less than 2 args")
(def err-use-argname "recover and provide functions must has same use argname attribute!")
(def err-recover-isfn "recover must be a function")
(def err-recover-overload "recover function must not has over-load arg lists")
(def err-recover-arg "recover function must has not less than 4 args")
(def err-arglist-equals "arglist of provide must equals to recover's")
(def err-argcount-duplicate "arg count of wraped function duplicate !")
(def err-recover-key "can not found recover for your recover data key !")

(defn bind-context [key context] (swap! contexts assoc key context))
(defn change-value [key value] (swap! *changed-values* assoc key value))

(defn use-argname? [fn-var]
  (let [arglist (-> (meta fn-var) :arglists first)]
    (and (>= (count arglist) 2)
         (= key-arg-name (name (nth arglist (- (count arglist) 2)))))))

(defn arg-list [fn-var remove-count]
  (-> (meta fn-var) :arglists first vec
      (#(subvec % 0 (- (count %) remove-count)))))

(defn check-context-fns
  [provide recover]
  (if (not (fn? (var-get provide))) (throw (RuntimeException. err-provide-isfn)))
  (if (not= 1 (count (:arglists (meta provide)))) (throw (RuntimeException. err-provide-overload)))
  (let [use-argname (use-argname? provide)
        provide-remove-arg-count (if use-argname 2 1)
        recover-remove-arg-count (if use-argname 4 3)]    
    (if (< (count (first (:arglists (meta provide)))) provide-remove-arg-count)
      (throw (RuntimeException. err-provide-arg)))
    (when recover
      (if (not (fn? (var-get recover)))(throw (RuntimeException. err-recover-isfn)))
      (if (not= 1 (count (:arglists (meta recover)))) (throw (RuntimeException. err-recover-overload)))
      (if (not= use-argname (use-argname? recover))  (throw (RuntimeException. err-use-argname)))
      (if (< (count (first (:arglists (meta recover)))) recover-remove-arg-count)
        (throw (RuntimeException. err-recover-arg)))
      (if (not= (arg-list provide provide-remove-arg-count)
                (arg-list recover recover-remove-arg-count))
        (throw (RuntimeException. err-arglist-equals))))))

(defn context-for [k contexts]
  (->> contexts
       vals
       (filter (fn [{:keys [key use-argname] :as context}]
                 (if (or (and use-argname (.startsWith (name k) key))
                         (and (not use-argname) (.equals (name k) key)))
                   context)))
       first))

(defn arg-name [arg context]
  (if (:use-argname context) (.substring (str arg) (.length (:key context)))))

(defn wraped-arglist [arglist contexts]
  (let [f-args (fn [arg]
                 (let [c (context-for arg contexts)]
                   (if c (map #(symbol (str arg "-" %)) (:arglist c)) [arg])))]
    (vec (mapcat f-args arglist))))

(defn wraped-meta
  [var-pure]
  (assoc (dissoc (meta var-pure) :wrapcontext) :arglists
         (->> (meta var-pure)
              :arglists
              (map #(wraped-arglist % @contexts)))))

(defn idx-ranges [idxs]
  (let [idxs (vec idxs)]
    (map (fn [i] {:start (apply + (subvec idxs 0 i))
                 :end (apply + (subvec idxs 0 (+ 1 i)))})
         (range (count idxs)))))

(defn arg-builders [arglist contexts]
  (let [cos (map (fn [arg] (context-for arg contexts)) arglist)
        idxs (map #(if % (count (:arglist %)) 1) cos)
        arg-count (apply + idxs)
        idx-rs (idx-ranges idxs)
        arg-names (map arg-name arglist cos)]
    [arg-count
     (map (fn [idx co arg-name idx-r]
            {:index idx
             :context co
             :arg-name arg-name
             :idx-range idx-r})
          (range (count arglist)) cos arg-names idx-rs)]))

(defn multi-arg-builders [arglists contexts]
  (let [m-builders (map (fn [arglist] (arg-builders arglist contexts)) arglists)]
    (if (not= (count m-builders) (count (distinct (map first m-builders))))
      (throw (RuntimeException. err-argcount-duplicate)))
    (into {} m-builders)))

(defn build-arg [{:keys [context arg-name idx-range]} args]
  (if context
    (let [invoke-args (-> (subvec args (:start idx-range) (:end idx-range))
                          (#(if (:use-argname context) (conj % arg-name) %))
                          (conj (:ops context)))]
      (apply (:provide context) invoke-args))
    (nth args (:start idx-range))))

(defn builder-for [k {:keys [arg-name context]}]
  (let [{:keys [key use-argname]} context]
    (or (and use-argname (.equals (name k) arg-name))
        (and (not use-argname) (.equals (name k) key)))))

(defn wrap-pure-function [var-f]
  (let [{:keys [arglists save]} (meta var-f)
        m-builders (multi-arg-builders arglists @contexts)
        f (var-get var-f)]
    (logging/debug (str "create wraped function of " var-f " arg builders is:" m-builders " save=" save))
    (fn [& wraped-args]
      (binding [*changed-values* (atom {})]
        (let [builders (m-builders (count wraped-args))]
          (if (nil? builders)
            (throw (IllegalArgumentException.
                    (str "Wrong number of args (" (count wraped-args)
                         ") passed to wraped-function of:" var-f))))
          (let [wraped-args (vec wraped-args)
                args (map #(build-arg % wraped-args) builders)
                result (apply f args)]
            (logging/trace "invoke wraped-function of:" var-f " wraped-args:" wraped-args " args:" args " result:" result)
            (if save (change-value save result))
            (doseq [[key value] @*changed-values*]
              (let [h (->> builders (filter #(builder-for key %)) first)]
                (logging/trace "recover data [key:" key " value:" value " recover:" h "]")
                (if (nil? h)
                  (throw (RuntimeException.
                          (str "can't find recover for your recover key ["
                               key "] when invoking wraped-function of:" var-f))))
                (let [{:keys [index context arg-name idx-range]} h
                      f-recover (:recover context)
                      args-recover (-> (subvec wraped-args (:start idx-range) (:end idx-range))
                                       (conj (nth args index) value)
                                       (#(if (:use-argname context) (conj % arg-name) %))
                                       (conj (:ops context)))]
                  (apply f-recover args-recover))))
            result))))))
