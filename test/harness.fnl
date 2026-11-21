;;; umbel test harness — a tiny assert-based framework.

(local fennel (require :fennel))
(local stats {:passed 0 :failed 0 :failures []})

(fn assert= [expected actual label]
  (let [ok (= expected actual)]
    (if ok
        (set stats.passed (+ stats.passed 1))
        (do
          (set stats.failed (+ stats.failed 1))
          (table.insert stats.failures
                        (.. label ": expected " (fennel.view expected) ", got "
                            (fennel.view actual)))))
    ok))

(fn assert-nil [actual label]
  (if (= nil actual)
      (set stats.passed (+ stats.passed 1))
      (do
        (set stats.failed (+ stats.failed 1))
        (table.insert stats.failures
                      (.. label ": expected nil, got " (fennel.view actual))))))

(fn assert-error [f label]
  (let [(ok err) (pcall f)]
    (if ok
        (do
          (set stats.failed (+ stats.failed 1))
          (table.insert stats.failures
                        (.. label ": expected an error, call succeeded")))
        (set stats.passed (+ stats.passed 1)))))

(fn deftest [name f]
  (let [before-failed stats.failed]
    (let [(ok err) (pcall f)]
      (when (not ok)
        (set stats.failed (+ stats.failed 1))
        (table.insert stats.failures (.. name ": CRASH " (fennel.view err)))))
    (when (= before-failed stats.failed)
      (print "  ok  " name))))

(fn summary []
  (print "")
  (print (.. stats.passed " passed, " stats.failed " failed"))
  (when (> stats.failed 0)
    (print "")
    (each [_ f (ipairs stats.failures)] (print "FAIL " f)))
  (if (> stats.failed 0) 1 0))

{: stats : assert= : assert-nil : assert-error : deftest : summary}
