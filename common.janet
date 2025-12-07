(defn floordiv
  "Rounds down the result of the division. Equivalent to Python's // operator"
  [a b]
  (math/floor (/ a b)))

(defn bool2num
  "Returns 1 if argument is true, 0 otherwise"
  [b]
  (if b 1 0))

(defn wrapmod
  "Returns a modulo b. Equivalent to Python's % operator"
  [a b]
  (- a (* b (floordiv a b))))

(defn find-max-index
  "Returns the index of the largest number in arrtup"
  [arrtup]
  (find-index |(= $ (max-of arrtup)) arrtup))

(defmacro memoize [form]
  "Memoize the result of evaluation of form"
  (def results @{})
  (defn memoize-call [f & args]
    (def memoization-key [f args])
    (if (has-key? results memoization-key)
      (in results memoization-key)
      (let [result (f ;args)]
        (put results memoization-key result)
        result)))
  ~(,memoize-call ,;form))
