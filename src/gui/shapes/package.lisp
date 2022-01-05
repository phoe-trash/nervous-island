(uiop:define-package #:nervous-island.gui.shapes
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:v #:vecto)
   (#:i #:imago)
   (#:z #:zpng))
  (:export #:*side* #:with-hex-tile
           #:module-background #:module-range #:module-circle
           #:melee #:ranged #:gauss
           #:net
           #:armor-shadow #:armor
           #:ability-circle
           #:text #:mobility #:toughness #:bomb
           ))
