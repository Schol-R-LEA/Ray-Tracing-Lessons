#!r6rs

(library (colors abstract-colors)
  (export make-color color?
          color=?
          blend-colors add-colors subtract-colors)

  (import (rnrs base (6))
          ;; composite standard lbrary, imports most std libs
          (rnrs (6)))

