;;;; test/state/space.lisp

(in-package #:nervous-island.test)

(define-test state-instantiation
  (let* ((board (apply #'nb:make-board (nc:range (nc:make-axial 0 0) 2)))
         (player-1 (make-instance 'np:player :army 'hegemony:army))
         (player-2 (make-instance 'np:player :army 'outpost:army))
         (phase (make-instance 'nph:start :player player-1))
         (state (make-instance 'nst:state :board board
                                          :players (list player-1 player-2)
                                          :current-phase phase)))
    (is eq board (nst:board state))
    (let ((players (nst:players state)))
      (is = 2 (length players))
      (true (member player-1 players))
      (true (member player-2 players)))
    (is eq '() (nst:alliances state))
    (let ((spaces (a:hash-table-keys (nst:spaces state))))
      (is = 19 (length spaces))
      (true (every (a:rcurry #'typep 'nc:axial) spaces)))
    (true (typep (nst:current-phase state) 'nph:start))
    (is eq '() (nst:previous-steps state))))
