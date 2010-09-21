;; lco emacs test client
;;
;; Copyright (C) 2010 Lyndon Tremblay <humasect@McHoovy.local>
;; Created: Thu Sep 16 10:32:40 MDT 2010
;;

(require 'json)
;; http://edward.oconnor.cx/2006/03/json.el

(defvar lco-server "localhost")
(defvar lco-port 1979)

(defconst lco-message-buffer-name "*lco-messages*")
(defconst lco-game-buffer-name "*lco-game*")
(defconst lco-splash "VRE client 1.0 ready.\n")

(defun message-buffer ()
  (get-buffer-create "*lco-messages*"))
(defun game-buffer ()
  (get-buffer-create "*lco-game*"))

(defun log (msg)
  (with-current-buffer (message-buffer)
  ;;(set-buffer (message-buffer))
  ;;(buffer-end 1)
    (set-window-point (get-buffer-window (message-buffer)) (point-max))
    (goto-char (point-max))
    (insert msg)))
  ;;(set-buffer (other-buffer)))

(defvar lco-process nil)

(defun lco-send (obj)
  (setq str (json-encode obj))
  (process-send-string lco-process str))

;;--------------------------------------------------------------
;; network
;;--------------------------------------------------------------

(defun lco-filter (proc string)
  (log string)
  (log (format "json: %s\n" (json-read-from-string string))))

(defun lco-sentinel (proc what)
  (log what))

(defun init-net (user pass)
  (log "Connecting... ")
  (setq lco-process (make-network-process
                     :name "lco-client"
                     :type nil
                     :host lco-server
                     :service lco-port
                     :family nil
                     :buffer (game-buffer)
                     :coding 'utf-8
                     :filter 'lco-filter
                     :sentinel 'lco-sentinel))
  (if lco-process
      (log "OK.\n")
    (log "Error.\n")))

;;--------------------------------------------------------------
;; display / interface
;;--------------------------------------------------------------

(defun init-display ()
  (setq f (make-frame
           '((title . lco-splash)
             (name . "lco-frame")
             (width . 80)
             (height . 50)
             (buffer-list . '((message-buffer) (game-buffer)))
             (unsplittable . t)
             (menu-bar-lines . nil)
             (tool-bar-lines . nil))))

  (select-frame f)
  (switch-to-buffer (message-buffer))
  ;;(delete-region (point-min) (point-max))
  (setq w2 (split-window (selected-window) 10))
  (select-window w2)
  (switch-to-buffer (game-buffer))
  ;;(delete-region (point-min) (point-max))
  )
  ;;(switch-to-buffer (game-buffer)))

(defun movement (angle)
  (lco-send '(:move angle)))

(defun init-keys ()
  (set-buffer (game-buffer))

  (local-set-key "4" (lambda () (interactive) (movement 270)))
  (local-set-key "2" (lambda () (interactive) (movement 180)))
  (local-set-key "6" (lambda () (interactive) (movement 90)))
  (local-set-key "8" (lambda () (interactive) (movement 0)))

  (local-set-key "7" (lambda () (interactive) (movement 315)))
  (local-set-key "9" (lambda () (interactive) (movement 45)))
  (local-set-key "3" (lambda () (interactive) (movement 135)))
  (local-set-key "1" (lambda () (interactive) (movement 225))))

;;--------------------------------------------------------------
;; API
;;--------------------------------------------------------------

(defun lco-init ()
  (log (format "\n%s" lco-splash))
  (init-keys))

(defun lco (user pass)
  (interactive "sUsername: \nsPassword: ")
  (init-display)
  (lco-init)

  (init-net user pass)
  (lco-send `(:client (:login [,user ,pass])))
  )

(defun lco-quit ()
  (interactive)
  (delete-process lco-process))

(defun lco-say (msg)
  (interactive "sMessage: ")
  (lco-send `(:client (:say msg))))
