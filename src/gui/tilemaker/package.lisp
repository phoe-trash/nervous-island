;;;; src/gui/tilemaker/package.lisp

(uiop:define-package #:nervous-island.gui.tilemaker
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:v #:vecto)
                    (#:φ #:phoe-toolbox)
                    (#:vs #:value-semantics-utils)
                    (#:na #:nervous-island.attack)
                    (#:ncom #:nervous-island.common)
                    (#:ne #:nervous-island.effect)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:shapes #:nervous-island.gui.shapes)))
