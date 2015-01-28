;;; nyan-mode.el --- Nyan Cat shows position in current buffer in mode-line.
;;;
;;; Nyanyanyanyanyanyanya!
;;;
;;; Author: Jacek "TeMPOraL" Zlydach <temporal.pl@gmail.com>
;;; URL: http://nyan-mode.buildsomethingamazing.com
;;; Version: 0.2
;;; Keywords: nyan, cat, lulz, pop tart cat, build something amazing
;;;
;;; Inspired by (and in few places copied from) sml-modeline.el,
;;; written by Lennart Borgman
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

;;; Some TODOs
;;; * Investigate why wavy rainbow didn't work on Antoszka's computer.
;;; * Refactor-out :set lambdas in customs if possible.
;;; * MAYBE add something to protect users from going to 0 with nyanbar width?
;;; * Add credits for used images.
(defgroup nyan nil
  "Customization group for `nyan-mode'."
  :group 'frames)

(defun nyan-refresh ()
  "Refresh after option changes if loaded."
  (when (featurep 'nyan-mode)
    (when (and (boundp 'nyan-mode)
               nyan-mode)
      (nyan-mode -1)
      (nyan-mode 1))))

(defcustom nyan-animation-frame-interval 0.2
  "Number of seconds between animation frames."
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defvar nyan-animation-timer nil)

(defun nyan-start-animation ()
  (interactive)
  (when (not (and nyan-animate-nyancat
		  nyan-animation-timer))
    (setq nyan-animation-timer (run-at-time "1 sec"
                                            nyan-animation-frame-interval
                                            'nyan-swich-anim-frame))
    (setq nyan-animate-nyancat t)))

(defun nyan-stop-animation ()
  (interactive)
  (when (and nyan-animate-nyancat
	     nyan-animation-timer)
    (cancel-timer nyan-animation-timer)
    (setq nyan-animation-timer nil)
    (setq nyan-animate-nyancat nil)))

;; mplayer needs to be installed for that
(defun nyan-start-music ()
  (interactive)
  (start-process-shell-command "nyan-music" "nyan-music" (concat "mplayer " +nyan-music+ " -loop 0")))
 
(defun nyan-stop-music ()
  (interactive)
  (kill-process "nyan-music"))

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
  "Length of Nyan Cat bar in units; each unit is equal to an 8px
  image. Minimum of 3 units are required for Nyan Cat."
  :set (lambda (sym val)
         (set-default sym val)
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-animate-nyancat nil
  "Enable animation for Nyan Cat.
This can be t or nil."
  :type '(choice (const :tag "Enabled" t)
                 (const :tag "Disabled" nil))
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (nyan-start-animation)
           (nyan-stop-animation))
         (nyan-refresh))
  :group 'nyan)

(defcustom nyan-cat-face-number 1
  "Select cat face number for console."
  )

(defconst +nyan-directory+ (file-name-directory (or load-file-name buffer-file-name)))

(defconst +nyan-cat-size+ 3)

(defconst +nyan-cat-image+ (concat +nyan-directory+ "img/nyan.xpm"))
(defconst +nyan-rainbow-image+ (concat +nyan-directory+ "img/rainbow.xpm"))
(defconst +nyan-outerspace-image+ (concat +nyan-directory+ "img/outerspace.xpm"))

(defconst +nyan-music+ (concat +nyan-directory+ "mus/nyanlooped.mp3"))

;;; Load images of Nyan Cat an it's rainbow.
(defvar nyan-cat-image (create-image +nyan-cat-image+ 'xpm nil :ascent 'center))

(defvar nyan-animation-frames (mapcar (lambda (id)
                                        (create-image (concat +nyan-directory+ (format "img/nyan-frame-%d.xpm" id))
                                                      'xpm nil :ascent 95))
                                      '(1 2 3 4 5 6)))
(defvar nyan-current-frame 0)

(defconst +catface+ [
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

(defun nyan-swich-anim-frame ()
  (setq nyan-current-frame (% (+ 1 nyan-current-frame) 6))
  (redraw-modeline))

(defun nyan-get-anim-frame ()
  (if nyan-animate-nyancat
      (nth nyan-current-frame nyan-animation-frames)
    nyan-cat-image))

(defun nyan-wavy-rainbow-ascent (number)
  (if nyan-animate-nyancat
      (min 100 (+ 90
                  (* 3 (abs (- (/ 6 2)
                               (% (+ number nyan-current-frame)
                                  6))))))
      (if (zerop (% number 2)) 80 'center)))

(defun nyan-number-of-rainbows ()
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               (- nyan-bar-length +nyan-cat-size+))
          100)))

(defun catface () (aref +catface+ nyan-cat-face-number))

(defun catface-index ()
  (min (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               (length (catface)))
          100)) (- (length (catface)) 1)))

(defun nyan-create ()
  (let* ((rainbows (nyan-number-of-rainbows))
         (outerspaces (- nyan-bar-length rainbows +nyan-cat-size+))
         (rainbow-string "")
         (nyancat-string (propertize
                          (aref (catface) (catface-index))
                          'display (nyan-get-anim-frame)))
         (outerspace-string ""))
    (dotimes (number rainbows)
      (setq rainbow-string (concat rainbow-string
                                   (propertize "|"
                                               'display (create-image +nyan-rainbow-image+ 'xpm nil :ascent (or (and nyan-wavy-trail

                                                                                                                     (nyan-wavy-rainbow-ascent number))
                                                                                                                (if nyan-animate-nyancat 95 'center)))))))
    (dotimes (number outerspaces)
      (setq outerspace-string (concat outerspace-string
                                      (propertize "-"
                                                  'display (create-image +nyan-outerspace-image+ 'xpm nil :ascent (if nyan-animate-nyancat 95 'center))))))
    ;; Compute Nyan Cat string.
    (concat rainbow-string
            nyancat-string
            outerspace-string)))

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


(provide 'nyan-mode)

;;; nyan-mode.el ends here
