;;; nyan-mode.el --- Nyan Cat shows position in current buffer in mode-line.
;;;
;;; Nyanyanyanyanyanyanya!
;;; 
;;; Author: Jacek "TeMPOraL" Zlydach <temporal.pl@gmail.com>
;;; URL: http://nyan-mode.buildsomethingamazing.com
;;; Keywords: nyan, cat, lulz, build something amazing

;;; FIXME INSERT GPL

;;; Most of the code here is stolen from/copied from/inspired by
;;; sml-modeline.el, written by Lennart Borgman
;;; See: http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/sml-modeline.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LICENSE
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

(defgroup nyan nil
  "Customization group for `nyan-mode'."
  :group 'frames)

;;; CARGO CULT CODE (copied from somewhere else, to be processed)
(defun nyan-refresh ()
  "Refresh after option changes if loaded."
  (when (featurep 'nyan-mode)
    (when (and (boundp 'nyan-mode)
               nyan-mode)
      (nyan-mode -1)
      (nyan-mode 1))))

(defcustom nyan-animate-nyancat t
  "Enable animation for Nyan Cat.
This can be t or nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

;;; TODO maybe customize background color and brackets or NyanCat.

;;; Load images of Nyan Cat an it's rainbow.
(defvar nyan-cat-image (create-image "~/nyan.png" 'png nil :ascent 'center))
(defvar nyan-rainbow-image (create-image "~/rainbow.png" 'png nil :ascent 'center))

;;; NOTE this function gets called pretty much every time an event
;;; (like keypress, or mousepress) occurs; if you have multiple
;;; frames, then this function gets automagically called several times
;;; per second (seems to be proportional to number of frames).

(defun nyan-rainbow-for-percentage (percentage)
 (let ((result ""))
   (dotimes (number
             (/ percentage 5))
     (setq result (concat result (propertize "nya" 'display (create-image "~/rainbow.png" 'png nil :ascent 'center)))))
   result))

(defun nyan-create ()
  (let ((percentage (round (* 100
                              (/ (- (float (point))
                                    (float (point-min)))
                                 (float (point-max)))))))
 ;; Compute: line/number, buffer length, percentage.
    (concat (format "%02d" percentage)
            (nyan-rainbow-for-percentage percentage)
            (propertize "NYAN NYAN NYAN"
                        'display nyan-cat-image))))

;;; 
;;; CARGO CULT WARNING I have no idea what it does, maybe will figure
;;; out later ;).
;;; Ok, does something with Emacs mode-line-position var.
;;; TODO figure it out.
;;; 
;;; I think it stores whatever was previously in the mode-line-position,
;;; in order to restore it when the mode is turned off.
(defvar nyan-old-car-mode-line-position nil)

;;;###autoload
(define-minor-mode nyan-mode
  "Use NyanCat to show buffer size and position in mode-line.
You can customize this minor mode, see option `nyan-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'nyan
  (if nyan-mode
      (progn
        (unless nyan-old-car-mode-line-position
          (setq nyan-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (nyan-create)))))
    (setcar mode-line-position nyan-old-car-mode-line-position)))


(provide 'nyan)