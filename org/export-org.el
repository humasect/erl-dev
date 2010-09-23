;; Export org files
;;
;; Copyright (C) 2010 Lyndon Tremblay <humasect@gmail.com>
;; Created: Tue Sep 21 03:51:43 MDT 2010
;;

(require 'org-publish)

(defconst zen-files-root "~/Hoovy/code/erl-dev/org")
(defconst zen-www-root "~/Hoovy/code/erl-dev/zen/www/dev")

(defun zen-publish ()
  (interactive)
  (let ((org-publish-project-alist
         `(
           ("zen-notes"
            :base-directory ,zen-files-root
            :base-extension "org"
            :publishing-function org-publish-org-to-html
            :publishing-directory ,zen-www-root
            :auto-sitemap t
            :sitemap-title "/Site Map/"
            :sitemal-alphabetically nil
            :recursive t
            :headline-levels 4
            :auto-preamble t)

           ("zen-static"
            :base-directory ,zen-files-root
            :base-extension "css\\|js\\|png\\|gif\\|pdf\\|mp3\\|ogg\\|svg"
            :publishing-function org-publish-attachment
            :publishing-directory ,zen-www-root
            :recursive t)

           ("zen" :components ("zen-notes" "zen-static")))))

    (org-publish-project "zen")))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-default-notes-file (concat zen-files-root "/notes.org"))
(define-key global-map (kbd "C-c c") 'org-capture)

(global-set-key (kbd "s-o") 'zen-publish)