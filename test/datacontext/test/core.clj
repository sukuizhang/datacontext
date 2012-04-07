(ns datacontext.test.core
  (:use [datacontext.core]
        [midje.sweet]))

(fact
  (default-name-strategy "xyz0") => "xyz"
  (default-name-strategy "inner-xyz") => "xyz"
  (default-name-strategy "xyz") => "xyz1")

(def datas (atom {}))

(defn data-provide [key1 key2 _]
  (get-in @datas [key1 key2]))

(defn data-restore [key1 key2 old-value new-value -]
  (swap! datas assoc-in [key1 key2] new-value))

(defn context-provide [arg-name _]
  (get-in @datas [:context (keyword arg-name)]))

(defn context-restore [old-value new-value arg-name _]
  (swap! datas assoc-in [:context (keyword arg-name)] new-value))

(def-context :data 'data-provide 'data-restore)
(def-context '-- 'context-provide 'context-restore)


(defn ^:wrapcontext get-data [data] data)

(defn
  ^{:wrapcontext true :save :data}
  add0 [data v]
  (+ (or data 0) v))

(defn ^{:wrapcontext true :save :data :fn-name 'wsub}
  sub [data v]
  (- (or data 0) v))

(defn ^:wrapcontext context0
  [--username --exp new-username new-exp]
  (set-value! :username new-username)
  (set-value! :exp new-exp)
  [--username --exp])

(wrap-pure-ns)

(fact
  (get-data1 :a :b) => nil
  (add :a :b 3) => 3
  (wsub :a :b -6) => 9
  (get-data1 :a :b) => 9
  (get-data1 :c :d) => nil
  (context "skz" 100) => [nil nil]
  (context "skz" 99) => ["skz" 100]
  (get-data1 :context :username) => "skz"
  (get-data1 :context :exp) => 99)
