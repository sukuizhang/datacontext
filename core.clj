(ns datacontext.test.context
  (:use [datacontext.context]
        [midje.sweet]))

(fact
  (arg-list (load-string "(defn a1 [a b c d e])") 2) => ['a 'b 'c])

(fact
  (check-context-fns (load-string "(def f1)") nil) => (throws RuntimeException err-provide-isfn)
  (check-context-fns (load-string "(defn p1 ([a]) ([]))") nil) => (throws RuntimeException err-provide-overload)
  (check-context-fns (load-string "(defn p2 ([a]))") nil) => (throws RuntimeException err-provide-arg)
  (let [p (load-string "(defn p3 [a b c arg-name ops])")]
    (check-context-fns p nil) => nil
    (check-context-fns p (load-string "(def f2)")) => (throws RuntimeException err-recover-isfn)
    (check-context-fns p (load-string "(defn r1 ([a]) ([]))")) => (throws RuntimeException err-recover-overload)
    (check-context-fns p (load-string "(defn r2 [a b c])")) => (throws RuntimeException err-recover-arg)
    (check-context-fns p (load-string "(defn r3 [a b d o-v n-v arg-name ops])")) => (throws RuntimeException err-arglist-equals)
    (check-context-fns p (load-string "(defn r4 [a b c o-v n-v arg-name ops])")) => nil))

(fact
  (let [contexts {:-- {:arg-name-prefix "--"}
                  :--- {:arg-name-prefix "---"}
                  :abc {:arg-name-prefix "abc"}}]
    (context-for '--xyz contexts) => {:arg-name-prefix "--"}
    (context-for '-xyz contexts) => nil
    (context-for 'abc contexts) => {:arg-name-prefix "abc"}))

(fact
  (wraped-arglist '(abc xyz h i j)
                  {:abc {:arg-name-prefix "abc" :arglist ['a 'b 'c]}
                   :xyz {:arg-name-prefix "xyz" :arglist ['x 'y 'z]}})
  => '[abc-a abc-b abc-c xyz-x xyz-y xyz-z h i j])

(fact
  (idx-ranges '(1 1 2 1 2 0 1 3)) => '({:start 0, :end 1} {:start 1, :end 2} {:start 2, :end 4} {:start 4, :end 5} {:start 5, :end 7} {:start 7, :end 7} {:start 7, :end 8} {:start 8, :end 11}))

(fact
  (let [context1 {:arg-name-prefix "arg1" :arglist '[a1 b1 c1]}
        context2 {:arg-name-prefix "--" :arglist '[]}
        context3 {:arg-name-prefix "arg3" :arglist '[a3 b3 c3]}
        contexts {:arg1 context1 :arg2 context2 :arg3 context3}
        [arg-count [b0 b1 b2 b3 b4 b5]] (arg-builder '(arg3 b --arg2 c arg1) contexts)]
    arg-count => 8
    b0 => (just {:index 0 :context context3 :arg-name {:name "arg3" :sub ""} :idx-range {:start 0 :end 3}})
    b1 => (just {:index 1 :context nil :arg-name nil :idx-range {:start 3 :end 4}})
    b2 => (just {:index 2 :context context2 :arg-name {:name "--arg2" :sub "arg2"} :idx-range {:start 4 :end 4}})
    b3 => (just {:index 3 :context nil :arg-name nil :idx-range {:start 4 :end 5}})
    b4 => (just {:index 4 :context context1 :arg-name {:name "arg1" :sub ""} :idx-range {:start 5 :end 8}})
    b5 => nil))

(fact
  (default-name-strategy "xyz0") => "xyz"
  (default-name-strategy "inner-xyz") => "xyz"
  (default-name-strategy "xyz") => "xyz1")
