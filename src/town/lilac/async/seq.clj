(ns town.lilac.async.seq)

(defmacro lazy-aseq
  [& body]
  (list 'new 'town.lilac.async.seq/AsyncLazySequence (list* 'fn* [] body) nil))
