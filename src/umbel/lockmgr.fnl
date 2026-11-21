;;; umbel.lockmgr — Strict Two-Phase Locking (2PL) with deadlock detection.
;;
;; Supports shared (:shared) and exclusive (:exclusive) locks on arbitrary
;; string resource keys (e.g. "tid:2:1", "page:2"), lock upgrades (S -> X),
;; wait-for graph cycle detection, cooperative blocking for concurrent
;; transaction coroutines, and atomic Strict-2PL release on commit/abort.

(fn new []
  {:locks {}       ;; resource -> {:mode :holders {txn->true} :waiters [...]}
   :txn-locks {}   ;; txn -> {resource -> true}
   :waiting-on {}}) ;; txn -> resource

(fn holder-count [entry]
  (var n 0)
  (each [_ _ (pairs entry.holders)] (set n (+ n 1)))
  n)

(fn record-lock [lm txn resource]
  (when (= (. lm.txn-locks txn) nil) (tset lm.txn-locks txn {}))
  (tset (. lm.txn-locks txn) resource true))

(fn can-grant? [entry txn mode]
  (if (or (= entry nil) (= (holder-count entry) 0))
      true
      (= entry.mode :exclusive)
      (not= nil (. entry.holders txn))
      ;; entry.mode == :shared
      (if (= mode :shared)
          ;; Don't starve an exclusive waiter already queued.
          (or (not= nil (. entry.holders txn)) (= (length entry.waiters) 0))
          ;; Upgrade S -> X allowed iff txn is the sole holder.
          (and (not= nil (. entry.holders txn)) (= (holder-count entry) 1)))))

(fn locked-by-other? [lm txn resource]
  "True when any transaction other than `txn` holds a lock on `resource`."
  (local entry (. lm.locks resource))
  (if (= entry nil)
      false
      (do (var other false)
          (each [h _ (pairs entry.holders)]
            (when (not= h txn) (set other true)))
          other)))

(fn grant! [lm txn resource mode]
  (local entry (or (. lm.locks resource)
                   (let [e {:mode mode :holders {} :waiters []}]
                     (tset lm.locks resource e)
                     e)))
  (when (= mode :exclusive) (set entry.mode :exclusive))
  (tset entry.holders txn true)
  (record-lock lm txn resource)
  true)

(fn would-deadlock? [lm txn resource]
  "DFS over the wait-for graph: does waiting on `resource` reach `txn`?"
  (local visited {})
  (fn visit-resource [res]
    (local entry (. lm.locks res))
    (if (= entry nil)
        false
        (do (var cycle false)
            (each [holder _ (pairs entry.holders)]
              (when (not cycle)
                (if (= holder txn)
                    (set cycle true)
                    (when (not (. visited holder))
                      (tset visited holder true)
                      (local next-res (. lm.waiting-on holder))
                      (when (and next-res (visit-resource next-res))
                        (set cycle true))))))
            cycle)))
  ;; Only check other holders (self-hold on upgrade isn't a cycle unless
  ;; another holder of `resource` waits on something `txn` holds).
  (local entry (. lm.locks resource))
  (if (= entry nil)
      false
      (do (var cycle false)
          (each [holder _ (pairs entry.holders)]
            (when (and (not cycle) (not= holder txn))
              (tset visited holder true)
              (local next-res (. lm.waiting-on holder))
              (when (and next-res (visit-resource next-res))
                (set cycle true))))
          cycle)))

(fn remove-waiter [entry co]
  (for [i (length entry.waiters) 1 -1]
    (when (= (. (. entry.waiters i) :co) co)
      (table.remove entry.waiters i))))

(fn acquire [lm txn resource mode opts]
  "Acquire :shared or :exclusive lock on `resource` for `txn`.
Yields when running in a concurrent coroutine; raises immediately on deadlock
or when opts.nowait is set (or called from the main thread)."
  (var acquired false)
  (while (not acquired)
    (local entry (. lm.locks resource))
    (if (can-grant? entry txn mode)
        (do (grant! lm txn resource mode)
            (set acquired true))
        (do (when (would-deadlock? lm txn resource)
              (error (.. "lockmgr: deadlock detected for txn " txn " on " resource)))
            (local (co main?) (coroutine.running))
            (if (or main? (and opts opts.nowait))
                (error (.. "lockmgr: lock conflict on " resource " for txn " txn))
                (do (tset lm.waiting-on txn resource)
                    (table.insert entry.waiters {:txn txn :mode mode :co co})
                    (coroutine.yield {:blocked-on resource :txn txn})
                    (tset lm.waiting-on txn nil)
                    (local cur (. lm.locks resource))
                    (when cur (remove-waiter cur co)))))))
  true)

(fn release-all [lm txn]
  "Strict 2PL release: drop every lock held by `txn`."
  (tset lm.waiting-on txn nil)
  (local held (. lm.txn-locks txn))
  (when held
    (each [res _ (pairs held)]
      (local entry (. lm.locks res))
      (when entry
        (tset entry.holders txn nil)
        (when (= (holder-count entry) 0)
          (if (= (length entry.waiters) 0)
              (tset lm.locks res nil)
              (set entry.mode :shared)))))
    (tset lm.txn-locks txn nil)))

(fn held-by [lm txn]
  (local out [])
  (when (. lm.txn-locks txn)
    (each [res _ (pairs (. lm.txn-locks txn))]
      (table.insert out res)))
  (table.sort out)
  out)

(fn run-concurrent [lm fns]
  "Drive a list of transaction thunks cooperatively as coroutines until all
finish (either succeeding or returning an error). Returns [{:ok :value} ...]."
  (local tasks
    (icollect [_ f (ipairs fns)]
      {:co (coroutine.create f) :done false :ok nil :value nil}))
  (var remaining (length tasks))
  (var progress true)
  (while (and (> remaining 0) progress)
    (set progress false)
    (each [_ task (ipairs tasks)]
      (when (not task.done)
        (local (ok res) (coroutine.resume task.co))
        (if (not ok)
            (do (set task.done true)
                (set task.ok false)
                (set task.value res)
                (set remaining (- remaining 1))
                (set progress true))
            (= (coroutine.status task.co) "dead")
            (do (set task.done true)
                (set task.ok true)
                (set task.value res)
                (set remaining (- remaining 1))
                (set progress true))
            (set progress true)))))
  tasks)

{:new new :acquire acquire :release-all release-all :held-by held-by
 :locked-by-other? locked-by-other? :run-concurrent run-concurrent}
