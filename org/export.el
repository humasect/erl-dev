;; Export org files
;;
;; Copyright (C) 2010 Lyndon Tremblay <humasect@gmail.com>
;; Created: Tue Sep 21 03:51:43 MDT 2010
;;

(require 'org-publish)

(defconst erl-root "~/Hoovy/code/erl-dev/")
(defconst erl-www "~/Hoovy/code/erl-dev/htdocs/")

(defun erl-make-notes (&optional name)
  `(,(concat name "-notes")
   :base-directory ,(concat erl-root "/" name "/org")
   :base-extension "org"
   :publishing-function org-publish-org-to-html
   :publishing-directory ,(concat erl-www "/dev/" name)
   :auto-sitemap t
   :sitemap-title "/Site Map/"
   :sitemap-alphabetically nil
   ;; :xml-declaration nil
   :recursive t

    ;;:style "<link rel=\"stylesheet\"
    ;;         type=\"text/css\" href=\"/docstyle.css\"/>"
   :style "<style type=\"text/css\">
html {
background: #555555;
color: #ffffff;
}
a:link { color: orange; }
a:visited { color: #aa6500; }
table { border-color: white; }
</style>"
   ;; :style "<style type=\"text/css\" media=\"screen\">
;; @import \"/docstyle.css\";
;; </style>"
   :style-include-default t

   :headline-levels 4
   :auto-preamble t))

(defun erl-make-static (&optional name)
  `(,(concat name "-static")
    :base-directory ,(concat erl-root "/" name "/org")
    :base-extension "css\\|js\\|png\\|gif\\|pdf\\|mp3\\|ogg\\|svg"
    :publishing-function org-publish-attachment
    :publishing-directory ,(concat erl-www "/dev/" name)
    :recursive t))

(defun erl-publish ()
  (interactive)
  (let ((org-publish-project-alist
         `(
           ,(erl-make-notes)
           ,(erl-make-static)
           ("erl" :components ("-notes" "-static"))

           ;;,(erl-make-notes "zen")
           ;;,(erl-make-static "zen")
           ;;("zen" :components ("zen-notes" "zen-static"))

           ;;,(erl-make-notes "vre")
           ;;,(erl-make-static "vre")
           ;;("vre" :components ("vre-notes" "vre-static"))

           ;;,(erl-make-notes "val")
           ;;,(erl-make-static "val")
           ;;("val" :components ("val-notes" "val-static"))

           )))

    ;;(org-publish-project "erl")
    ;;(org-publish-project "zen")
    ;;(org-publish-project "vre")
    (org-publish-project "erl")))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-default-notes-file (concat erl-root "/org/notes.org"))
(define-key global-map (kbd "C-c c") 'org-capture)
(add-to-list 'org-export-html-inline-image-extensions "svg")

(global-set-key (kbd "s-o") 'erl-publish)
