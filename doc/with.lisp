;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(defun movies-from-director (movie)
  (let ((other-movies))
    (facts:with ((?director :directed movie
                            :directed ?other-movie))
      (push ?other-movie other-movies))
    other-movies))
