(ns datacontext.core
  (:require [datacontext.context :as context]
            [datacontext.concept :as concept]
            [clojure.tools.logging :as logging]))

(defn def-context
  "定义一个数据上下文。
   available: 用来判断数据上下文是否适合于某个key，可以直接指定一个函数，也可以指定一个string，symbol或keyweod，这样将要求key以(name available)开头。
   provide:   指定一个provide函数，必须传入var或者可以访问到这个函数的symbol。
   recover:   指定一个recover函数，必须传入var或者可以访问到这个函数的symbol。
   ops:       指定一个在执行provide和recover函数时使用的ops参数。"
  [available provide recover & [ops]]
  (let [provide (or (and (var? provide) provide) (load-string (str "#'" provide)))
        recover (or (and (var? recover) recover) (load-string (str "#'" recover)))]
    (logging/debug (str "binding context [available:" available
                        " provide:" provide " recover:" recover " ops:" ops "]"))
    (context/bind-context (concept/build-datacontext available provide recover ops))))

(defn set-value!
  "声明需要回收数据。
   key:      需要回收的数据的key。
   data:     回收的数据。
   coors:    回收数据的坐标，在key为参数名的时候，自动用接口函数参数值作为坐标，在其它情况则可以在这里指定回收坐标。"
  [key data & coors]
  (context/change-value key data coors) nil)

(defn default-name-strategy
  "默认的函数名策略，函数名策略确定由逻辑函数名生成接口函数名的规则。"
  [fn-name]
  (cond
   (.endsWith fn-name "0") (.substring fn-name 0 (- (count fn-name) 1))
   (.startsWith fn-name "inner-") (.substring fn-name (count "inner-"))
   :else (str fn-name "1")))

(defn wrap-pure-ns
  "在指定命名空间下找到适合的逻辑函数，生成接口函数，并且把它们放到(另一)指定的命名空间。
   默认情况下，查找逻辑函数和放置接口函数的命名空间都是当前命名空间。
   此外还可以替换掉默认的函数名策略。"
  [& {:keys [ns to-ns name-strategy]
      :or {name-strategy default-name-strategy}}]
  (let [pure-ns (or (and  ns (the-ns (symbol ns))) *ns*)
        wraped-ns (or (and to-ns (create-ns (symbol to-ns))) pure-ns)
        to-be-wraped (->> (ns-publics pure-ns)
                (filter (fn [[sym var]]
                         (and (var? var)
                              (fn? (var-get var))
                              (concept/keyword-wrapcontext (meta var))))))]
    (doseq [[sym var] to-be-wraped]
      (let [f (context/wrap-pure-function var)
            fn-name (or ((meta var) :fn-name) (name-strategy (name sym)))
            fn-sym (with-meta (symbol fn-name) (context/inf-fn-meta @@#'context/contexts var))]
        (intern wraped-ns fn-sym f)))))
