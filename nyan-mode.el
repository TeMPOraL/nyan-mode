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

(defcustom nyan-wavy-trail nil
  "If enabled, Nyan Cat's rainbow trail will be wavy."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

;;; FIXME refactor (out) the lambda.
;;; TODO add some security for going with nyan below 0.
(defcustom nyan-bar-length 20
  "Length of Nyan Cat bar in units; each unit is equal to an 8px
  image. Minimum of 3 units are required for Nyan Cat."
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

;;; Yeah, maybe one day.
;; (defcustom nyan-animate-nyancat nil
;;   "Enable animation for Nyan Cat.
;; This can be t or nil."
;;   :type '(choice (const :tag "Enabled" t)
;;                  (const :tag "Disabled" nil))
;;   :set (lambda (sym val)
;;          (set-default sym val)
;;          (nyan-refresh))
;;   :group 'nyan)

;;; TODO maybe customize background color and brackets or NyanCat.

(defconst +nyan-cat-size+ 3)

(defconst +nyan-cat-image+ "~/nyan.png")
(defconst +nyan-rainbow-image+ "~/rainbow.png")
(defconst +nyan-outerspace-image+ "~/outerspace.png")

;;; Load images of Nyan Cat an it's rainbow.
(defvar nyan-cat-image (create-image +nyan-cat-image+ 'png nil :ascent 'center))

;;; NOTE this function gets called pretty much every time an event
;;; (like keypress, or mousepress) occurs; if you have multiple
;;; frames, then this function gets automagically called several times
;;; per second (seems to be proportional to number of frames).
(defun nyan-create ()
  (let* ((percentage (round (* 100
                               (/ (- (float (point))
                                     (float (point-min)))
                                  (float (point-max))))))
         (rainbows (round (/ (* percentage (- nyan-bar-length +nyan-cat-size+))
                             100)))
         (outerspaces (- nyan-bar-length rainbows +nyan-cat-size+))
         (rainbow-string "")
         (nyancat-string (propertize "NYAN NYAN NYAN"
                                     'display nyan-cat-image))
         (outerspace-string ""))
    (dotimes (number rainbows)
      (setq rainbow-string (concat rainbow-string
                                   (propertize "nya"
                                               'display (create-image +nyan-rainbow-image+ 'png nil :ascent (if (and nyan-wavy-trail
                                                                                                                     (zerop (% number 2)))
                                                                                                                80
                                                                                                              'center))))))
    (dotimes (number outerspaces)
      (setq outerspace-string (concat outerspace-string
                                      (propertize "nya"
                                                  'display (create-image +nyan-outerspace-image+ 'png nil :ascent 'center)))))
 ;; Compute: line/number, buffer length, percentage.
    (concat ;(format "%02d" percentage)
            rainbow-string
            nyancat-string
            outerspace-string)))

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