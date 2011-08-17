;;; sml-modeline.el --- Show position in a scrollbar like way in mode-line
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-03-16 Tue
;; Version: 0.5
;; Last-Updated: 2010-03-18 Thu
;; URL: http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/sml-modeline.el
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Show scrollbar like position indicator in mode line.
;; See the global minor mode `sml-modeline-mode' for more information.
;;
;; Idea and part of this code is adapted from David Engster's and Drew
;; Adam's code in these mail messages:
;;
;;   http://lists.gnu.org/archive/html/emacs-devel/2010-03/msg00523.html
;;   http://permalink.gmane.org/gmane.emacs.devel/122038
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defgroup sml-modeline nil
  "Customization group for `sml-modeline-mode'."
  :group 'frames)

(defun sml-modeline-refresh ()
  "Refresh after option changes if loaded."
  (when (featurep 'sml-modeline)
    (when (and (boundp 'sml-modeline-mode)
               sml-modeline-mode)
      (sml-modeline-mode -1)
      (sml-modeline-mode 1))))

(defcustom sml-modeline-len 12
  "Mode line indicator total length."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (sml-modeline-refresh))
  :group 'sml-modeline)

(defcustom sml-modeline-borders nil
  "Indicator borders.
This is a pair of indicators, like [] or nil."
  :type '(choice (const :tag "None" nil)
                 (cons (string :tag "Left border")
                       (string :tag "Right border")))
  :set (lambda (sym val)
         (set-default sym val)
         (sml-modeline-refresh))
  :group 'sml-modeline)

(defcustom sml-modeline-numbers 'percentage
  "Position number style.
This can be 'percentage or 'line-number."
  :type '(choice (const :tag "Line numbers" line-numbers)
                 (const :tag "Percentage" percentage))
  :set (lambda (sym val)
         (set-default sym val)
         (sml-modeline-refresh))
  :group 'sml-modeline)

(defface sml-modeline-end-face
  '((t (:inherit match)))
  "Face for invisible buffer parts."
  :group 'sml-modeline)
;; 'face `(:background ,(face-foreground 'mode-line-inactive)
;;         :foreground ,(face-background 'mode-line))

(defface sml-modeline-vis-face
  '((t (:inherit region)))
  "Face for invisible buffer parts."
  :group 'sml-modeline)
;; 'face `(:background ,(face-foreground 'mode-line)
;;         :foreground ,(face-background 'mode-line))

;;(sml-modeline-create)
(defun sml-modeline-create ()
 (let* ((wstart (window-start))
        (wend (window-end))
        number-max number-beg number-end
        (sml-begin (or (car sml-modeline-borders) ""))
        (sml-end   (or (cdr sml-modeline-borders) ""))
        (inner-len (- sml-modeline-len (length sml-begin) (length sml-end)))
        bpad-len epad-len
        pos-%
        start end
        string)
   (if (not (or (< wend (save-restriction (widen) (point-max)))
                (> wstart 1)))
       ""
     (cond
      ((eq sml-modeline-numbers 'percentage)
       (setq number-max (save-restriction (widen) (point-max)))
       (setq number-beg (/ (float wstart) (float number-max)))
       (setq number-end (/ (float wend) (float number-max)))
       (setq start (floor (* number-beg inner-len)))
       (setq end (floor (* number-end inner-len)))
       (setq string
             (concat (format "%02d" (round (* number-beg 100)))
                     "-"
                     (format "%02d" (round (* number-end 100))) "%%")))
      ((eq sml-modeline-numbers 'line-numbers)
       (save-restriction
         (widen)
         (setq number-max (line-number-at-pos (point-max)))
         (setq number-beg (line-number-at-pos wstart))
         (setq number-end (line-number-at-pos wend)))
       (setq start (floor (* (/ number-beg (float number-max)) inner-len)))
       (setq end   (floor (* (/ number-end (float number-max)) inner-len)))
       (setq string
             (concat "L"
                     (format "%02d" number-beg)
                     "-"
                     (format "%02d" number-end))))
      (t (error "Unknown sml-modeline-numbers=%S" sml-modeline-numbers)))
     (setq inner-len (max inner-len (length string)))
     (setq bpad-len (floor (/ (- inner-len (length string)) 2.0)))
     (setq epad-len (- inner-len (length string) bpad-len))
     (setq pos-% (+ bpad-len (length string) -1))
     (setq string (concat sml-begin
                          (make-string bpad-len 32)
                          string
                          (make-string epad-len 32)
                          sml-end))
     ;;(assert (= (length string) sml-modeline-len) t)
     (when (= start sml-modeline-len) (setq start (1- start)))
     (setq start (+ start (length sml-begin)))
     (when (= start end) (setq end (1+ end)))
     (when (= end pos-%) (setq end (1+ end))) ;; If on % add 1
     (put-text-property start end 'face 'sml-modeline-vis-face string)
     (when (and (= 0 (length sml-begin))
                (= 0 (length sml-end)))
       (put-text-property 0 start 'face 'sml-modeline-end-face string)
       (put-text-property end sml-modeline-len 'face 'sml-modeline-end-face string))
     string)))

(defvar sml-modeline-old-car-mode-line-position nil)

;;;###autoload
(define-minor-mode sml-modeline-mode
  "Show buffer size and position like scrollbar in mode line.
You can customize this minor mode, see option `sml-modeline-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'sml-modeline
  (if sml-modeline-mode
      (progn
        (unless sml-modeline-old-car-mode-line-position
          (setq sml-modeline-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (sml-modeline-create)))))
    (setcar mode-line-position sml-modeline-old-car-mode-line-position)))


(provide 'sml-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sml-modeline.el ends here
