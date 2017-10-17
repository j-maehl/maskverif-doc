((easycrypt-mode .
  ((eval .
    (flet ((pre (s) (concat (locate-dominating-file buffer-file-name ".dir-locals.el") s)))
           (setq easycrypt-load-path `(,(pre ".") ,(pre "Order2") ,(pre "Order3") ,(pre "Order4") ,(pre "Order5") ,(pre "Order6") ,(pre "Order7"))))))))
