(ns datacontext.context
  (:require [clojure.tools.logging :as logging]
            [datacontext.concept :as concept]))

(def ^:private contexts (atom []))
(def ^{:dynamic true :private true} *changed-values* (atom {}))

(defn bind-context [c]
  (swap! contexts (fn [cs] (->> cs (filter #(not= (:available-fn %) (:available-fn c))) (cons c)))))
(defn change-value [key data & [coors]]
  (swap! *changed-values* assoc key {:coors coors :data data}))

(defn inf-fn-arglist
  "计算逻辑函数一个参数表对应的接口函数的参数表。
   规则是把所有找到DataContext的参数换成DataContext的coor-args列表。"
  [contexts arglist]
  (let [f-args (fn [arg]
                 (let [c (concept/data-context-for contexts arg)]
                   (if c (map #(symbol (str arg "-" %)) (:arglist c)) [arg])))]
    (vec (mapcat f-args arglist))))

(defn inf-fn-meta
  "构建接口函数的meta，遵循以下几个规则:
   1.把逻辑函数的每个参数表映根据找到的DataContext映射成接口函数的参数表。
   2.去掉:wrapcontext。
   3.其它所有meta原样保留。"
  [contexts var-fn-logic]
  (let [new-arglists (->> (meta var-fn-logic)
                          :arglists
                          (map #(inf-fn-arglist contexts %)))]
    (-> (meta var-fn-logic)
        (dissoc concept/keyword-wrapcontext)
        (assoc :arglists new-arglists))))

(defn logic-fn-arg
  "计算逻辑函数的参数值。
   如果对应的DataContext不为nil，调用DataContext的provide方法获得参数值，反之则直接返回对应的接口函数参数。"
  [^ArgContext {:keys [context use-key coor-arg-index-range]} inf-fn-args]
  (if context
    (let [invoke-args (-> (subvec inf-fn-args (:start coor-arg-index-range) (:end coor-arg-index-range))
                          (concat (if (:use-key? context) [use-key]))
                          (concat [(:ops context)]))]
      (apply (:provide context) invoke-args))
    (nth inf-fn-args (:start coor-arg-index-range))))

(defn recover-data
  "回收数据。
  arg-contexts:  参数上下文ArgContext列表,用来回收以参数名作为key的数据。
  data-contexts: 数据上下文DataContext列表，用来回收非参数名做key的数据。
  var-fn-logic:  逻辑函数。
  inf-args:      接口函数传进来的参数值表。
  logic-args:    由接口函数参数值表通过provide计算得到的逻辑函数参数值表。
  key:           回收数据的key。
  value:         回收的数据，形式为{:coors coors :data data},data是需要回收的数据，coors是数据的坐标，在不是参数名做key是有用"
  [arg-contexts data-contexts var-fn-logic inf-args logic-args key value] 
  (let [recover (or (concept/arg-context-for arg-contexts key)
                    (concept/data-context-for data-contexts key))
        {:keys [coors data]} value]
    (logging/trace (pr-str "recover data [key:" key " value:" value " recover:" recover "]"))
    (cond (= datacontext.concept.DataContext (type recover))
          (let [invoke-args (-> (or coors (take (count (:coor-args recover)) (repeat nil)))
                                (concat (if (:use-key? recover) [nil]))
                                (concat [nil data (:ops recover)]))]
            (apply (:recover recover) invoke-args))
          (= datacontext.concept.ArgContext (type recover))
          (let [{:keys [index context use-key coor-arg-index-range]} recover
                invoke-args (-> (subvec inf-args (:start coor-arg-index-range) (:end coor-arg-index-range))
                                (concat (if (:use-key? context) [use-key]))
                                (concat [(nth logic-args index) data (:ops context)]))]
            (apply (:recover context) invoke-args))
          :else
          (throw (RuntimeException.
                  (str "can't find recover for your recover key ["
                       key "] when invoking wraped-function of:" var-fn-logic))))))

(defn wrap-pure-function
  "由逻辑函数创建一个接口函数，执行时把接口函数穿进来的坐标参数转换成逻辑函数的数据参数，并执行逻辑函数，执行完之后回收要求更改的数据。"
  [var-fn-logic]
  (let [{:keys [arglists save]} (meta var-fn-logic)
        fn-arg-contexts (concept/fn-arg-contexts @contexts arglists)
        f (var-get var-fn-logic)]
    (logging/trace "create inf function of " var-fn-logic " fn arg contexts is:" fn-arg-contexts " save=" save)
    (fn [& inf-args]
      (binding [*changed-values* (atom {})]
        (let [arg-contexts (fn-arg-contexts (count inf-args))]
          (if (nil? arg-contexts)
            (throw (IllegalArgumentException.
                    (str "Wrong number of args (" (count inf-args)
                         ") passed to wraped-function of:" var-fn-logic))))
          (let [inf-args (vec inf-args)
                logic-args (map #(logic-fn-arg % inf-args) arg-contexts)
                result (apply f logic-args)]
            (logging/trace "invoke inf-function of:" var-fn-logic " inf-fn-args:" inf-args " logic args:" logic-args " result:" result)
            (if save (change-value save result))
            (doseq [[key value] @*changed-values*]
              (change-value key (recover-data arg-contexts @contexts var-fn-logic inf-args logic-args key value)))
            (or (and save (get-in @*changed-values* [save :data])) result)))))))
