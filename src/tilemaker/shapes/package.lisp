(uiop:define-package #:nervous-island.tilemaker.shapes
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:v #:vecto)
   (#:i #:imago)
   (#:z #:zpng))
  (:export #:*side* #:with-hex-tile
           #:melee #:ranged #:gauss
           #:net
           #:armor-shadow #:armor
           #:ability-circle
           #:text #:mobility #:toughness #:bomb))
