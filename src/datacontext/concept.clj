(ns datacontext.concept
  (:require [clojure.tools.logging :as logging]))

(defonce keyword-wrapcontext :wrapcontext)

(def context-fns-require "provide和recover都是只有一个参数表的函数，参数表第一部分是Coordinate arg list，provide和recover具有相同的Coordinate arg list;随后紧跟一个数据key的参数，用来提供辅助坐标信息；此外，对于provide，后面还需要1个表示options的参数，对recover，后面还需要3个依此表示旧值，新值和options的参数。")

;DataContext表示一个数据上下文，由def-context声明。
;provide           提供数据函数
;recover           回收数据函数
;available-fn      用来判断DataContext是否适用于某个key的函数。
;coor-args         坐标参数的列表
;ops               选项，在def-context时声明，并且作为实际提供数据和回收数据时的参数。
(defrecord DataContext [provide recover available-fn coor-args ops])
(defn mk-data-context [provide recover available-fn coor-args ops]
  (DataContext. provide recover available-fn coor-args ops))

(def
 ^{:doc "创建默认的available-fn,根据指定的prefix，当key以prefix表示对这个key可用。"}
 mk-default-available-fn
     (memoize (fn [prefix] (fn [key] (.startsWith (name key) prefix)))))

(defn data-context-for
  "从DataContext集合中查找合适某个key的DataContext"
  [contexts key]
  (->> contexts
       (filter #((:available-fn %) key))
       first))

(defn coor-arg-list
  "获取context-fn中的数据坐标参数的列表"
  [fn-var n-coor-arg-count]
  (-> (meta fn-var) :arglists first vec
      (#(subvec % 0 (- (count %) n-coor-arg-count)))))

(defn check-context-fns
  "检查创建datacontext的provide和recover是否合法"
  [provide recover]
  (and (fn? (var-get provide))
       (fn? (var-get recover))
       (= 1 (count (:arglists (meta provide))))
       (= 1 (count (:arglists (meta recover))))
       (>= (count (first (:arglists (meta provide)))) 2)
       (>= (count (first (:arglists (meta recover)))) 4)
       (= (coor-arg-list provide 2)
          (coor-arg-list recover 4))))

(defn available-fn
  "构建一个判断DataContext是否适合某个key的函数。
   本身是函数直接使用，是symbol，keyword或字符窜，则作为prefix用来构造一个默认的available-fn"
  [a]
  (or (and (fn? a) a)
      (and (or (string? a) (symbol? a) (keyword? a)) (mk-default-available-fn (name a)))
      (throw (RuntimeException. "available必须是函数或可以转化为clojure.lang.Named"))))

(defn build-datacontext
  "构建一个数据上下文"
  [available provide recover ops]
  (or (and (check-context-fns provide recover)
           (mk-data-context (var-get provide)
                   (var-get recover)
                   (available-fn available) (coor-arg-list provide 2) ops))
    (throw (RuntimeException. context-fns-require))))

;执行接口函数时，DataContext根据接口函数的参数值逐个地构造出逻辑函数参数值去执行逻辑函数，这就需要建立两组(坐标和数据)参数之间联系，同样，当回收更新数据时也依赖于这种联系，这就叫做参数上下文。
;每个逻辑函数参数都会以自己的名字做key来查找到适合它们的DataContext，由此建立参数上下文。
;如果未能查找到合适的DataContext，那么说明这个逻辑参数不是需要注入的数据，这时参数上下文只是简单拷贝来自接口函数的参数值。
;index                    对应逻辑函数的第几个参数
;argname                  对应逻辑函数的参数名
;context                  使用的DataContext
;coor-arg-index-range     对应的来自接口函数的坐标参数的索引范围，context为nil的时候仅表示需要拷贝的接口参数的位置。
(defrecord ArgContext [index argname context coor-arg-index-range])
(defn mk-arg-context [index argname context coor-arg-index-range]
  (ArgContext. index argname context coor-arg-index-range))

(defn count-to-range
  "把数量的列表转换成范围:
   [1 2 2] -> [{:start 0 :end 1} {:start 1 :end 3} {:start 3 :end 5}]"
  [counts]
  (let [counts (vec counts)]
    (map (fn [i] {:start (apply + (subvec counts 0 i))
                 :end (apply + (subvec counts 0 (+ 1 i)))})
         (range (count counts)))))

(defn arg-contexts
  "构建逻辑函数的一个参数表对应的ArgContext列表。
   返回的结果为[接口函数对应参数表的长度, ArgContext列表]
  contexts: 注册的所有DataContext。
  arglist:  逻辑函数的一个参数表，一个clojure函数可能有多个参数表。"
  [contexts arglist]
  (let [cs (map (fn [arg] (data-context-for contexts arg)) arglist)
        indexs (range (count arglist))
        argnames (map name arglist)
        coor-arg-counts (map #(if % (count (:coor-args %)) 1) cs)
        coor-arg-index-ranges (count-to-range coor-arg-counts)]
    [(apply + coor-arg-counts)
     (map mk-arg-context indexs argnames cs coor-arg-index-ranges)]))

(defn fn-arg-contexts
  "构建一个逻辑函数的所有参数表的ArgContext列表。
   返回结果为接口函数的对应参数表长度和对应ArgContext列表的map。
   如果逻辑函数有其中2个参数表对应的接口函数参数表长度，则抛出异常。
   contexts: 注册的所有DataContext。
   arglists: 逻辑函数的所有参数列表。"
  [contexts arglists]
  (let [fn-contexts (map (fn [arglist] (arg-contexts contexts arglist)) arglists)]
    (if (not= (count fn-contexts) (count (distinct (map first fn-contexts))))
      (throw (RuntimeException. "要求创建的接口函数具有参数表长度相等的函数体。")))
    (into {} fn-contexts)))

(defn arg-context-for
  "从ArgContext集合中查找合适某个key的ArgContext"
  [contexts key]
  (->> contexts
       (filter #(.equals (:argname %) (name key)))
       first))
