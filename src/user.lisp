;;;; src/user.lisp

(uiop:define-package #:nervous-island.user
  (:use #:cl)
  ;; External packages
  (:local-nicknames (#:a #:alexandria))
  ;; Nervous Island packages
  (:local-nicknames (#:na #:nervous-island.attack)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:nch #:nervous-island.choice)
                    (#:ncom #:nervous-island.common)
                    (#:ne #:nervous-island.effect)
                    (#:ni #:nervous-island.instant)
                    (#:np #:nervous-island.player)
                    (#:nph #:nervous-island.phase)
                    (#:nr #:nervous-island.army)
                    (#:ns #:nervous-island.skill)
                    (#:nsp #:nervous-island.space)
                    (#:nst #:nervous-island.state)
                    (#:nt #:nervous-island.tile))
  ;; Nervous Island armies
  (:local-nicknames (#:moloch #:nervous-island.armies.moloch)
                    (#:outpost #:nervous-island.armies.outpost)
                    (#:borgo #:nervous-island.armies.borgo)
                    (#:hegemony #:nervous-island.armies.hegemony)))

(in-package #:nervous-island.user)
