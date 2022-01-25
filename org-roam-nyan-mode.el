;;; org-roam-nyan-mode.el --- Nyan Cat shows how many ideas you are trying to create each day.

;; Nyanyanyanyanyanyanya!

;; Author: Ran Wang
;; URL:
;; Version:
;; Keywords: nyan, cat, progress bar, org-roam

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Contributions and feature requests welcome!

;; Inspired by Nyan-mode written by Jacek "TeMPOraL" Zlydach.
;; See: https://github.com/TeMPOraL/nyan-mode/

;;; History:

;;; Code:

;;;; Requirements

(require 'ts)

(defcustom zk-reminder-string "Wk:"
  "Information need to keep in mind."
  :type 'string
  :group 'nyan)

(defcustom zk-reminder-date-string "2021-01-01"
  "By setting date, the modeline will show how many weeks past
from a date."
  :type 'string
  :group 'nyan)

(defun zk-how-many-weeks-from-a-date ()
  (/ (round (/ (- (ts-unix (ts-now))
                  (ts-unix (ts-parse zk-reminder-date-string)))
               86400))
     7))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun today-effort-str ()
  "Return a string of how many ideas generated today"
  (substring (get-string-from-file "~/.emacs.d/nyan-mode/log.today_effort") 0 -1))

(defun today-effort ()
  "Check how many ideas generated today"
  (float (string-to-number (today-effort-str))))

(defcustom zk-daily-goal 10
  "Number of seconds between animation frames."
  :type 'float
  :group 'nyan)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun org-roam-mode-line-need-more-effort ()
  (when (string-match-p (expand-file-name org-roam-directory) (format "%s" buffer-file-name))
    (progn (setq mode-line-format
                 (list
                  '(:eval
                    (window-numbering-get-number-string))
                  " "
                  'mode-line-mule-info
                  'mode-line-modified
                  'mode-line-frame-identification
                  "  "
                  '(:eval (list (nyan-create)))
                  "|"
                  "%n"
                  " Today: "
                  '(:eval (today-effort-str))
                  "  Goal: "
                  '(:eval (format "%s" zk-daily-goal))
                  "   "
                  " | "
                  zk-reminder-string
                  (format "%s" (zk-how-many-weeks-from-a-date))
                  'global-mode-string
                  ))
           (force-mode-line-update)
           )))

(add-hook 'before-save-hook #'org-roam-mode-line-need-more-effort)

(defconst +nyan-directory+ (file-name-directory (or load-file-name buffer-file-name)))

(defconst +nyan-cat-size+ 3)

(defconst +nyan-cat-image+ (concat +nyan-directory+ "img/nyan.xpm"))
(defconst +nyan-rainbow-image+ (concat +nyan-directory+ "img/rainbow.xpm"))
(defconst +nyan-outerspace-image+ (concat +nyan-directory+ "img/outerspace.xpm"))

(defconst +nyan-modeline-help-string+ "Nyanyanya!\nmouse-1: Scroll buffer position")

(defvar nyan-old-car-mode-line-position nil)

(defgroup nyan nil
  "Customization group for `nyan-mode'."
  :group 'frames)

(defun nyan-refresh ()
  "Refresh nyan mode.
Intended to be called when customizations were changed, to reapply them immediately."
  (when (featurep 'nyan-mode)
    (when (and (boundp 'nyan-mode)
               nyan-mode)
      (nyan-mode -1)
      (nyan-mode 1))))

(defcustom nyan-animation-frame-interval 0.2
  "Number of seconds between animation frames."
  :type 'float
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defvar nyan-animation-timer nil)

(defun nyan--is-animating-p ()
  "T if animating, NIL otherwise."
  (timerp nyan-animation-timer))

(defun nyan-start-animation ()
  (interactive)
  (unless (nyan--is-animating-p)
    (setq nyan-animation-timer (run-at-time "1 sec"
                                            nyan-animation-frame-interval
                                            'nyan-swich-anim-frame))))

(defun nyan-stop-animation ()
  (interactive)
  (when (nyan--is-animating-p)
    (cancel-timer nyan-animation-timer)
    (setq nyan-animation-timer nil)))

(defcustom nyan-minimum-window-width 64
  "Minimum width of the window, below which nyan-mode will not be displayed.
This is important because nyan-mode will push out all informations from small windows."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

;;; FIXME bug, doesn't work for antoszka.
(defcustom nyan-wavy-trail nil
  "If enabled, Nyan Cat's rainbow trail will be wavy."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-bar-length 32
  "Length of Nyan Cat bar in units.
Each unit is equal to an 8px image.
Minimum of 3 units are required for Nyan Cat."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-animate-nyancat nil
  "Enable animation for Nyan Cat.
This can be t or nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  ;; FIXME: Starting an animation timer on defcustom isn't a good idea; this needs to, at best, maybe start/stop a timer iff the mode is on,
  ;; otherwise just set a flag. -- Jacek Złydach, 2020-05-26
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (nyan-start-animation)
           (nyan-stop-animation))
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-cat-face-number 1
  "Select cat face number for console."
  :type 'integer
  :group 'nyan
  )

;;; Load images of Nyan Cat an it's rainbow.
(defvar nyan-cat-image (if (image-type-available-p 'xpm)
                           (create-image +nyan-cat-image+ 'xpm nil :ascent 'center)))

(defvar nyan-animation-frames (if (image-type-available-p 'xpm)
                                  (mapcar (lambda (id)
                                            (create-image (concat +nyan-directory+ (format "img/nyan-frame-%d.xpm" id))
                                                          'xpm nil :ascent 95))
                                          '(1 2 3 4 5 6))))
(defvar nyan-current-frame 0)

(defconst +nyan-catface+ [
                          ["[]*" "[]#"]
                          ["(*^ｰﾟ)" "( ^ｰ^)" "(^ｰ^ )" "(ﾟｰ^*)"]
                          ["(´ω｀三 )" "( ´ω三｀ )" "( ´三ω｀ )" "( 三´ω｀)"
                           "( 三´ω｀)" "( ´三ω｀ )" "( ´ω三｀ )" "(´ω｀三 )"]
                          ["(´д｀;)" "( ´д`;)" "( ;´д`)" "(;´д` )"]
                          ["(」・ω・)」" "(／・ω・)／" "(」・ω・)」" "(／・ω・)／"
                           "(」・ω・)」" "(／・ω・)／" "(」・ω・)」" "＼(・ω・)／"]
                          ["(＞ワ＜三　　　)" "(　＞ワ三＜　　)"
                           "(　　＞三ワ＜　)" "(　　　三＞ワ＜)"
                           "(　　＞三ワ＜　)" "(　＞ワ三＜　　)"]])

(defun nyan-toggle-wavy-trail ()
  "Toggle the trail to look more like the original Nyan Cat animation."
  (interactive)
  (setq nyan-wavy-trail (not nyan-wavy-trail)))

(defun nyan-swich-anim-frame ()
  (setq nyan-current-frame (% (+ 1 nyan-current-frame) 6))
  (redraw-modeline))

(defun nyan-get-anim-frame ()
  (if (nyan--is-animating-p)
      (nth nyan-current-frame nyan-animation-frames)
    nyan-cat-image))

(defun nyan-wavy-rainbow-ascent (number)
  (if (nyan--is-animating-p)
      (min 100 (+ 90
                  (* 3 (abs (- (/ 6 2)
                               (% (+ number nyan-current-frame)
                                  6))))))
    (if (zerop (% number 2)) 80 'center)))

(defun nyan-number-of-rainbows ()
  (round (/ (* (round (* 100
                         (if (> (/ (today-effort) zk-daily-goal) 1)
                             (float 1)
                           (/ (today-effort) zk-daily-goal))
                         ))
               (- nyan-bar-length +nyan-cat-size+))
            100)))

(defun nyan-catface ()
  (aref +nyan-catface+ nyan-cat-face-number))

(defun nyan-catface-index ()
  (min (round (/ (* (round (* 100
                              (/ (- (float (point))
                                    (float (point-min)))
                                 (float (point-max)))))
                    (length (nyan-catface)))
                 100)) (- (length (nyan-catface)) 1)))

(defun nyan-scroll-buffer (percentage buffer)
  "Move point `BUFFER' to `PERCENTAGE' percent in the buffer."
  (interactive)
  (with-current-buffer buffer
    (goto-char (floor (* percentage (point-max))))))

(defun nyan-add-scroll-handler (string percentage buffer)
  "Propertize `STRING' to scroll `BUFFER' to `PERCENTAGE' on click."
  (lexical-let ((percentage percentage)
                (buffer buffer))
    (propertize string 'keymap `(keymap (mode-line keymap (down-mouse-1 . ,(lambda () (interactive) (nyan-scroll-buffer percentage buffer))))))))

(defun nyan-create ()
  "Return the Nyan Cat indicator to be inserted into mode line."
  (if (< (window-width) nyan-minimum-window-width)
      ""                                ; disabled for too small windows
    (let* ((rainbows (nyan-number-of-rainbows))
           (outerspaces (- nyan-bar-length rainbows +nyan-cat-size+))
           (rainbow-string "")
           (xpm-support (image-type-available-p 'xpm))
           (nyancat-string (propertize
                            (aref (nyan-catface) (nyan-catface-index))
                            'display (nyan-get-anim-frame)))
           (outerspace-string "")
           (buffer (current-buffer)))
      (dotimes (number rainbows)
        (setq rainbow-string (concat rainbow-string
                                     (nyan-add-scroll-handler
                                      (if xpm-support
                                          (propertize "|"
                                                      'display (create-image +nyan-rainbow-image+ 'xpm nil :ascent (or (and nyan-wavy-trail
                                                                                                                            (nyan-wavy-rainbow-ascent number))
                                                                                                                       (if (nyan--is-animating-p) 95 'center))))
                                        "|")
                                      (/ (float number) nyan-bar-length) buffer))))
      (dotimes (number outerspaces)
        (setq outerspace-string (concat outerspace-string
                                        (nyan-add-scroll-handler
                                         (if xpm-support
                                             (propertize "-"
                                                         'display (create-image +nyan-outerspace-image+ 'xpm nil :ascent (if (nyan--is-animating-p) 95 'center)))
                                           "-")
                                         (/ (float (+ rainbows +nyan-cat-size+ number)) nyan-bar-length) buffer))))
      ;; Compute Nyan Cat string.
      (propertize (concat rainbow-string
                          nyancat-string
                          outerspace-string)
                  'help-echo +nyan-modeline-help-string+))))



;;;###autoload
(define-minor-mode nyan-mode
  "Use NyanCat to show buffer size and position in mode-line.
You can customize this minor mode, see option `nyan-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'nyan
  ;; FIXME: That doesn't smell right; might still get duplicate nyan cats and other mode-line disruptions.  -- Jacek Złydach, 2020-05-26
  (cond (nyan-mode
         (unless nyan-old-car-mode-line-position
           (setq nyan-old-car-mode-line-position (car mode-line-position)))
         (setcar mode-line-position '(:eval (list (nyan-create))))
         ;; NOTE Redundant, but intended to, in the future, prevent the custom variable from starting the animation timer even if nyan mode isn't active. -- Jacek Złydach, 2020-05-26
         (when nyan-animate-nyancat
           (nyan-start-animation)))
        ((not nyan-mode)
         (nyan-stop-animation)          ; In case there was an animation going on.
         (setcar mode-line-position nyan-old-car-mode-line-position)
         (setq nyan-old-car-mode-line-position nil))))


(provide 'org-roam-nyan-mode)

;;; org-roam-nyan-mode.el ends here
