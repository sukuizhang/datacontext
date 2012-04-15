(ns datacontext.test.context
  (:use [datacontext.context]
        [datacontext.concept]
        [midje.sweet]))



(fact
  (defn ^{:private true :wrapcontext true} l1 ([abc1 xyz1]) ([a b abc]))
  (let [contexts [{:available-fn (mk-default-available-fn "abc") :arglist ['a 'b 'c]}
                  {:available-fn (mk-default-available-fn "xyz") :arglist ['x 'y 'z]}]
        arglist0 (inf-fn-arglist contexts '(abc xyz h i j))
        {:keys [private wrapcontext arglists]} (inf-fn-meta contexts #'l1)]
    arglist0 => '[abc-a abc-b abc-c xyz-x xyz-y xyz-z h i j]
    arglists => '([abc1-a abc1-b abc1-c xyz1-x xyz1-y xyz1-z] [a b abc-a abc-b abc-c])
    private => true
    wrapcontext => nil))

(fact
  (defn p [x y --key ops] (str x y --key (:v ops)))
  (let [c {:provide p :ops {:v "ops"}}
        ac1 {:coor-arg-index-range {:start 0 :end 1}}
        ac2 {:coor-arg-index-range {:start 1 :end 3} :context c :argname "-argname-"}
        inf-fn-args ["inf-arg" "abc" "xyz"]]
    (logic-fn-arg ac1 inf-fn-args) => "inf-arg"
    (logic-fn-arg ac2 inf-fn-args) => "abcxyz-argname-ops"))
