(ns thi.ng.common.data.byteutils
  #+clj
  (:import
   [java.io OutputStream InputStream]))

#+clj (defn int->byte [x] (if (> x 0x7f) (- x 0x100) x))
#+clj (defn byte->int [x] (if (neg? x) (+ x 0x100) x))

#+clj
(defn read-int16-le
  [^InputStream in]
  (let [buf (byte-array 2)]
    (.read in buf 0 2)
    (bit-or (byte->int (aget buf 0)) (bit-shift-left (byte->int (aget buf 1)) 8))))
#+cljs
(defn read-int16-le
  [in])

#+clj
(defn read-int32-le
  [^InputStream in]
  (let [buf (byte-array 4)]
    (.read in buf 0 4)
    (bit-or
     (byte->int (aget buf 0))
     (bit-shift-left (byte->int (aget buf 1)) 8)
     (bit-shift-left (byte->int (aget buf 2)) 16)
     (bit-shift-left (byte->int (aget buf 3)) 24))))
#+cljs
(defn read-int32-le
  [in])

#+clj
(defn read-float-le
  [^InputStream in]
  (Float/intBitsToFloat (unchecked-int (read-int32-le in))))
#+cljs
(defn read-float-le
  [in])

#+clj
(defn read-vec3-le
  [^InputStream in]
  [(read-float-le in) (read-float-le in) (read-float-le in)])
#+cljs
(defn read-ve3-le
  [in])

#+clj
(defn write-str-bytes
  [^OutputStream out ^String x]
  (.write out (.getBytes x))
  out)
#+cljs
(defn write-str-bytes
  [out x])

#+clj
(defn write-int16-le
  [^OutputStream out x]
  (.write
   out
   (byte-array
    [(unchecked-byte (bit-and x 0xff))
     (unchecked-byte (bit-and (bit-shift-right x 8) 0xff))]))
  out)
#+cljs
(defn write-int16-le
  [out x])

#+clj
(defn write-int32-le
  [^OutputStream out x]
  (.write
   out
   (byte-array
    [(unchecked-byte (bit-and x 0xff))
     (unchecked-byte (bit-and (bit-shift-right x 8) 0xff))
     (unchecked-byte (bit-and (bit-shift-right x 16) 0xff))
     (unchecked-byte (bit-shift-right x 24))]))
  out)
#+cljs
(defn write-int32-le
  [out x])

#+clj
(defn write-float-le
  [^OutputStream out x]
  (write-int32-le out (Float/floatToRawIntBits x)))
#+cljs
(defn write-float-le
  [out x])

#+clj
(defn write-vec3-le
  [^OutputStream out [x y z]]
  (write-float-le out x)
  (write-float-le out y)
  (write-float-le out z))
#+cljs
(defn write-vec3-le
  [out [x y z]])
