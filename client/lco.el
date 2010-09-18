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
  (set-buffer (message-buffer))
  (insert msg)
  (set-buffer (other-buffer)))

(defvar lco-process nil)

(defun lco-send (obj)
  (setq str (json-encode obj))
  (process-send-string lco-process str))

(defun lco-filter (proc string)
  (log string))

(defun init-net (user pass)
;;make-network...
;;set-process-buffer process buffer
  (log "Connecting... ")

  (setq lco-process (make-network-process
                     :name "lco-client"
                     :type nil
                     :host lco-server
                     :service lco-port
                     :family nil
                     :buffer (message-buffer)
                     :coding 'utf-8
                     :filter 'lco-filter))
  (if lco-process
      (log "OK.\n")
    (log "Error.\n")))

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
  (switch-to-buffer (game-buffer))
  (setq w2 (split-window (selected-window) 38))
  (select-window w2)
  (switch-to-buffer (message-buffer)))

(defun init-keys ()
  (set-buffer (game-buffer))
  (local-set-key "x" '(lambda ()
                       (log "diddsdsds x.\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lco-init ()
  (log lco-splash)
  (init-keys))

(defun lco (user pass)
  (interactive "sUsername: \nsPassword: ")
  (init-display)
  (lco-init)

  (init-net user pass)
  (lco-send '(:login [user pass]))
  )

(defun lco-quit ()
  (delete-process lco-process))
