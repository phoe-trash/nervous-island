;;;; src/user.lisp

(uiop:define-package #:nervous-island.user
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)
                    (#:ne #:nervous-island.effect)
                    (#:ni #:nervous-island.instant)
                    (#:nr #:nervous-island.army)
                    (#:ns #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:moloch #:nervous-island.armies.moloch)
                    (#:outpost #:nervous-island.armies.outpost)
                    (#:borgo #:nervous-island.armies.borgo)
                    (#:hegemony #:nervous-island.armies.hegemony)))

(in-package #:nervous-island.user)