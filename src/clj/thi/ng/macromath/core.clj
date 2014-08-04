(ns thi.ng.macromath.core)

(defmacro defmathop
  "Constructs macro to build inlined nested expressions which when
  call will apply f successively to all args. Supports arities 2-8."
  [name f]
  `(defmacro ~name
     ([a# b#]
        `(~~f ~a# ~b#))
     ([a# b# c#]
        `(~~f (~~f ~a# ~b#) ~c#))
     ([a# b# c# d#]
        `(~~f (~~f (~~f ~a# ~b#) ~c#) ~d#))
     ([a# b# c# d# e#]
        `(~~f (~~f (~~f (~~f ~a# ~b#) ~c#) ~d#) ~e#))
     ([a# b# c# d# e# f#]
        `(~~f (~~f (~~f (~~f (~~f ~a# ~b#) ~c#) ~d#) ~e#) ~f#))
     ([a# b# c# d# e# f# g#]
        `(~~f (~~f (~~f (~~f (~~f (~~f ~a# ~b#) ~c#) ~d#) ~e#) ~f#) ~g#))
     ([a# b# c# d# e# f# g# h#]
        `(~~f (~~f (~~f (~~f (~~f (~~f (~~f ~a# ~b#) ~c#) ~d#) ~e#) ~f#) ~g#) ~h#))))

(defmacro defmathop2
  "Constructs macro to build inlined nested expressions which when
  call will apply f to inner pairs and f2 to combine results."
  [name f f2]
  `(defmacro ~name
     ([a# b# c#]
        `(~~f2 (~~f ~a# ~b#) ~c#))
     ([a# b# c# d#]
        `(~~f2 (~~f ~a# ~b#) (~~f ~c# ~d#)))
     ([a# b# c# d# e#]
        `(~~f2 (~~f2 (~~f ~a# ~b#) (~~f ~c# ~d#)) ~e#))
     ([a# b# c# d# e# f#]
        `(~~f2 (~~f2 (~~f ~a# ~b#) (~~f ~c# ~d#)) (~~f ~e# ~f#)))
     ([a# b# c# d# e# f# g#]
        `(~~f2 (~~f2 (~~f2 (~~f ~a# ~b#) (~~f ~c# ~d#)) (~~f ~e# ~f#)) ~g#))
     ([a# b# c# d# e# f# g# h#]
        `(~~f2 (~~f2 (~~f2 (~~f ~a# ~b#) (~~f ~c# ~d#)) (~~f ~e# ~f#)) (~~f ~g# ~h#)))))

(defmacro defmathop3
  "Takes f, f2 & f3 as syntax-quoted symbols. Constructs a macro which
  when called, applies f to all but the last 1 or 2 args. The
  remaining arg(s) are combined with the first result using f2.
  Furthermore, for arities 6 and 8, f3 is first applied to the last
  two args are before the final application of f2. For example:

      (defmathop* maddsub `madd `- `*)
      (maddsub 2 3 4 5) => (- (madd 2 3 4) 5)
      (maddsub 2 3 4 5 6) => (- (madd 2 3 4) (* 5 6))"
  [name f f2 f3]
  `(defmacro ~name
     ([a# b# c# d#]
        `(~~f2 (~~f ~a# ~b# ~c#) ~d#))
     ([a# b# c# d# e#]
        `(~~f2 (~~f ~a# ~b# ~c# ~d#) ~e#))
     ([a# b# c# d# e# f#]
        `(~~f2 (~~f ~a# ~b# ~c# ~d#) (~~f3 ~e# ~f#)))
     ([a# b# c# d# e# f# g#]
        `(~~f2 (~~f ~a# ~b# ~c# ~d# ~e# ~f#) ~g#))
     ([a# b# c# d# e# f# g# h#]
        `(~~f2 (~~f ~a# ~b# ~c# ~d# ~e# ~f#) (~~f3 ~g# ~h#)))))

(defmathop add `+)
(defmathop sub `-)
(defmathop mul `*)
(defmathop div `/)
(defmathop2 madd `* `+)
(defmathop2 msub `* `-)
(defmathop2 addm `+ `*)
(defmathop2 subm `- `*)
(defmathop2 adddiv `+ `/)
(defmathop2 subdiv `- `/)
(defmathop3 maddsub `madd `- `*)
(defmathop3 addmsub `addm `- `*)
(defmathop3 msubadd `msub `+ `*)
(defmathop3 submadd `subm `+ `*)

(defmacro if*
  "Returns y if x > 0, else 0"
  [pred x y] `(if (~pred ~x) ~y 0))

(defmacro bitmask
  "Constructs a bit mask from given values & predicate fn applied to
  each. If pred returns truthy value the value's related bit is set.
  Bit values start at 1 and double for successive args (max 8)."
  ([pred a]
     `(if* ~pred ~a 0x01))
  ([pred a b]
     `(bit-or (bitmask ~pred ~a) (if* ~pred ~b 0x02)))
  ([pred a b c]
     `(bit-or (bitmask ~pred ~a ~b) (if* ~pred ~c 0x04)))
  ([pred a b c d]
     `(bit-or (bitmask ~pred ~a ~b ~c) (if* ~pred ~d 0x08)))
  ([pred a b c d e]
     `(bit-or (bitmask ~pred ~a ~b ~c ~d) (if* ~pred ~e 0x10)))
  ([pred a b c d e f]
     `(bit-or (bitmask ~pred ~a ~b ~c ~d ~e) (if* ~pred ~f 0x20)))
  ([pred a b c d e f g]
     `(bit-or (bitmask ~pred ~a ~b ~c ~d ~e ~f) (if* ~pred ~g 0x40)))
  ([pred a b c d e f g h]
     `(bit-or (bitmask ~pred ~a ~b ~c ~d ~e ~f ~g) (if* ~pred ~h 0x80))))
