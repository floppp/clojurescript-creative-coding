(ns helpers.macros)

(defmacro half [v]
  `(/ ~v 2))

(defmacro twice [v]
  `(* ~v 2))
