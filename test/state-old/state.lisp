;;;; test/state/space.lisp

(in-package #:nervous-island.test)

(define-test state-instantiation
  (flet ((test (&rest args)
           (let* ((player-1 (make-instance 'np:player :army 'hegemony:army))
                  (player-2 (make-instance 'np:player :army 'outpost:army))
                  (phase (make-instance 'nph:start :player player-1 :number 1))
                  (state (apply #'make-instance 'nst:state
                                :players (list player-1 player-2)
                                :current-phase phase args)))
             (true (typep (nst:board state) 'nb:board))
             (let ((players (nst:players state)))
               (is = 2 (length players))
               (true (member player-1 players))
               (true (member player-2 players))
               (true (member :hegemony players
                             :key (a:compose #'nr:name #'np:army)))
               (true (member :outpost players
                             :key (a:compose #'nr:name #'np:army))))
             (is eq '() (nst:alliances state))
             (let ((spaces (a:hash-table-keys (nst:spaces state))))
               (is = 19 (length spaces))
               (true (every (a:rcurry #'typep 'nc:axial) spaces)))
             (true (typep (nst:current-phase state) 'nph:start))
             (is eq '() (nst:previous-steps state)))))
    (test)
    (test :board :standard)
    (test :board (apply #'nb:make-board (nc:range (nc:axial 0 0) 2)))))
