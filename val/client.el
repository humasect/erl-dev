;; lco emacs test client
;;
;; Copyright (C) 2010 Lyndon Tremblay <humasect@McHoovy.local>
;; Created: Thu Sep 16 10:32:40 MDT 2010
;;

(global-set-key (kbd "s-.") 'lco)

(require 'json)
;; http://edward.oconnor.cx/2006/03/json.el

(defvar lco-server "localhost")
(defvar lco-port 1979)

(defconst lco-message-buffer-name "*lco-messages*")
(defconst lco-game-buffer-name "*lco-game*")
(defconst lco-splash "VRE client 1.0 ready.\n")

(defun lco-message-buffer ()
  (get-buffer-create "*lco-messages*"))
(defun lco-game-buffer ()
  (get-buffer-create "*lco-game*"))

(defun lco-log (msg)
  (with-current-buffer (lco-message-buffer)
  ;;(set-buffer (message-buffer))
  ;;(buffer-end 1)
    (set-window-point (get-buffer-window (lco-message-buffer)) (point-max))
    (goto-char (point-max))
    (insert msg)))
  ;;(set-buffer (other-buffer)))


(defun lco-change-room (room)
  (lco-log (format "change room! %s" room))
  (with-current-buffer (lco-game-buffer)
    (erase-buffer)))

;;--------------------------------------------------------------
;; network
;;--------------------------------------------------------------

(defvar lco-process nil)

(defun lco-send (obj)
  (setq str (json-encode obj))
  (process-send-string lco-process str))

(defun lco-filter (proc string)
  (lco-log string)
  (let* ((msg (car (json-read-from-string string)))
         (name (car msg))
         (args (cdr msg)))
    (cond ((eq name 'change_room) (lco-change-room))
          (t (lco-log (format "json: %s\n" msg)))
          )))

(defun lco-sentinel (proc what)
  (lco-log what))

(defun lco-init-net (user pass)
  (lco-log "Connecting... ")
  (setq lco-process (make-network-process
                     :name "lco-client"
                     :type nil
                     :host lco-server
                     :service lco-port
                     :family nil
                     :buffer (lco-game-buffer)
                     :coding 'utf-8
                     :filter 'lco-filter
                     :sentinel 'lco-sentinel))
  (if lco-process
      (lco-log "OK.\n")
    (lco-log "Error.\n")))

;;--------------------------------------------------------------
;; display / interface
;;--------------------------------------------------------------

(defun lco-init-display ()
  (setq f (make-frame
           '((title . lco-splash)
             (name . "lco-frame")
             (width . 80)
             (height . 50)
             (buffer-list . '((lco-message-buffer) (lco-game-buffer)))
             (unsplittable . t)
             (menu-bar-lines . nil)
             (tool-bar-lines . nil))))

  (select-frame f)
  (switch-to-buffer (lco-message-buffer))
  (setq buffer-read-only t)
  ;;(delete-region (point-min) (point-max))
  (setq w2 (split-window (selected-window) 10))
  (select-window w2)
  (switch-to-buffer (lco-game-buffer))
  ;;(delete-region (point-min) (point-max))
  )
  ;;(switch-to-buffer (game-buffer)))

(defun lco-move (angle)
  ;;(interactive "nAngle: ")
  (lco-send `(:move ,angle)))

(defun lco-init-keys ()
  (set-buffer (lco-game-buffer))

  (local-set-key "/" (lambda (cmd)
                       (interactive "sCommand: ")
                       (lco-send `(:command ,cmd))))

  (local-set-key "t" (lambda (msg)
                       (interactive "sMessage: ")
                       (lco-send `(:client (:say ,msg)))))

  (local-set-key "q" 'lco-quit)

  (local-set-key "4" (lambda () (interactive) (lco-move 270)))
  (local-set-key "2" (lambda () (interactive) (lco-move 180)))
  (local-set-key "6" (lambda () (interactive) (lco-move 90)))
  (local-set-key "8" (lambda () (interactive) (lco-move 0)))

  (local-set-key "7" (lambda () (interactive) (lco-move 315)))
  (local-set-key "9" (lambda () (interactive) (lco-move 45)))
  (local-set-key "3" (lambda () (interactive) (lco-move 135)))
  (local-set-key "1" (lambda () (interactive) (lco-move 225))))

;;--------------------------------------------------------------
;; API
;;--------------------------------------------------------------

(defun lco-init ()
  (lco-log (format "\n%s" lco-splash))
  (lco-init-keys))

(defun lco (user pass)
  (interactive "sUsername: \nsPassword: ")
  (lco-init-display)
  (lco-init)

  (lco-init-net user pass)
  (lco-send `(:login [,user ,pass "japanese"])))

(defun lco-quit ()
  (interactive)
  (lco-log "Quit.")
  (delete-process lco-process))


