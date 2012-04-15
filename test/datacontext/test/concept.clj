(ns datacontext.test.concept
  (:use [datacontext.concept]
        [midje.sweet]))

(fact
  (use-key? (load-string "(defn u1 [a b c d e])") 2) => false
  (use-key? (load-string "(defn u2 [a b c --key e])") 2) => true)

(fact
  (coor-arg-list (load-string "(defn a1 [a b c d e])") 2) => '[a b c])

(defn available-fn [key] (when (.equals (name key) "abc") "abc"))
(fact
  (let [context1 {:available-fn (mk-default-available-fn "--")}
        context2 {:available-fn available-fn}
        contexts [context1 context2]]
    (data-context-for contexts '--xyz) => context1
    (data-context-for contexts '-xyz) => nil
    (data-context-for contexts 'abc) => context2
    (data-context-for contexts 'abc1) => nil))

(def p1 (load-string "(defn pr1 [a b c ops])"))
(def r1 (load-string "(defn re1 [a b c o-v n-v ops])"))
(def p2 (load-string "(defn pr2 [a b c --key ops])"))
(def r2 (load-string "(defn re2 [a b c --key o-v n-v ops])"))
(fact
  (check-context-fns (load-string "(def pdef)") r1) => false
  (check-context-fns p1 (load-string "(def rdef)")) => false
  (check-context-fns (load-string "(defn pov ([a])([a b]))") r1) => false
  (check-context-fns p1 (load-string "(defn rov ([a]) ([]))")) => false
  (check-context-fns (load-string "(defn pargcount [])") r1) => false
  (check-context-fns p1 (load-string "(defn rargcount [a b])")) => false
  (check-context-fns p1 (load-string "(defn key-list [a b d o-v n-v ops])")) => false
  (check-context-fns p1 r1) => true
  (check-context-fns p1 r2) => false
  (check-context-fns p2 r1) => false
  (check-context-fns p2 r2) => true)

(fact
  (let [c1 (build-datacontext '-- p2 r2 {})
        c2 (build-datacontext available-fn p1 r1 {})]
    c1 => (contains {:use-key? true :coor-args '[a b c] :ops {}})
    c2 => (contains {:use-key? false :coor-args '[a b c] :ops {}})
    ((:available-fn c1) "--name") => "name"
    (= (:available-fn c2) available-fn) => true))    

(fact
  (count-to-range '(1 1 2 1 2 0 1 3)) => '({:start 0, :end 1} {:start 1, :end 2} {:start 2, :end 4} {:start 4, :end 5} {:start 5, :end 7} {:start 7, :end 7} {:start 7, :end 8} {:start 8, :end 11}))

(fact
  (let [c1 {:available-fn (mk-default-available-fn "xy") :coor-args '[x y]}
        c2 {:available-fn (mk-default-available-fn "abc") :coor-args '[a b c]}
        c3 {:available-fn (mk-default-available-fn "-") :coor-args '[]}
        [argcount [ac0 ac1 ac2 ac3 ac4]] (arg-contexts [c1 c2 c3] '[u xy1 abc1 -u-inf abc2])]
    argcount => 9
    ac0 => (contains {:index 0 :argname "u" :use-key nil :coor-arg-index-range {:start 0 :end 1}}) 
    ac1 => (contains {:index 1 :argname "xy1" :use-key "1" :coor-arg-index-range {:start 1 :end 3}})
    ac2 => (contains {:index 2 :argname "abc1" :use-key "1" :coor-arg-index-range {:start 3 :end 6}})
    ac3 => (contains {:index 3 :argname "-u-inf" :use-key "u-inf" :coor-arg-index-range {:start 6 :end 6}})
    ac4 => (contains {:index 4 :argname "abc2" :use-key "2" :coor-arg-index-range {:start 6 :end 9}})))

(fact
  (let [ac1 {:argname "xyz"}
        ac2 {:argname "--abc" :context {:use-key? true} :use-key "abc"}
        ac3 {:argname "--def" :context {:use-key? false} :use-key "def"}]
    (arg-context-of? ac1 "xyz") => true
    (arg-context-of? ac2 "--abc") => true
    (arg-context-of? ac2 "abc") => true
    (arg-context-of? ac3 "def") => false
    (arg-context-for [ac1 ac2 ac3] "xyz") => ac1
    (arg-context-for [ac1 ac2 ac3] "abc") => ac2
    (arg-context-for [ac1 ac2 ac3] "mn") => nil))
