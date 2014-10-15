;;; guerrilla.el --- A guerrilla warfare game

;; Copyright (C) 2014 Paul Rankin

;; Author: Paul Rankin <paul@tilk.co>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 's)

(defgroup guerrilla ()
  "A guerrilla warfare game."
  :prefix "guerrilla-"
  :group 'games
  :link '(url-link "https://github.com/rnkn/guerrilla"))

(defgroup guerrilla-mapping ()
  "Mapping options for `guerrilla'."
  :prefix "guerrilla-map-"
  :group 'guerrilla)

;; Constants ===========================================================

(defconst guerrilla-splash
  "\
                                __ __ __
  .-----.--.--.-----.----.----.|__|  |  |.---.-.
  |  _  |  |  |  -__|   _|   _||  |  |  ||  _  |
  |___  |_____|_____|__| |__|  |__|__|__||___._|
  |_____|======================================

   by Paul W. Rankin
"
  "Title splash art.")

(defconst guerrilla-map-topography
  "\
**:::::::*:::::::..................:::::::********::::::::..:::::::***::\
:**::**::::::::.............. . ....:::::::*****:::*::::.....:::::***:::\
**::***:::*:.::::..::::............:::::::::::***:::::........:::*:*****\
.:**::*:::::.::.::.::..............:::::::**:***:::::::.....::::**:**:::\
**.***::*::*:::::.::::.*..:.....::*::::::***:*:::*::::..:..::*:*******::\
:*****:::::**:::::::.::::::.. ..:::::::::*******:::::.:::::****:*****:::\
..***:*:::****:**:*:**..:::......:*:::::***:***::::::::::***:****:***::*\
..**:*::::**********::::::......:::::::::::*****:::*::::*::********:::::\
.:::::::::********::*::::::::...::*:::::********::::::::**********:*::::\
.::.::::::*****:*:::*:.:::::...:::::::::::*****:::::*:*::****::***::::::\
..:.:::::::::***:::::::::::.....::::::::**::*:::**:*::********::::::::::\
..:::::::*:**:*::::::::...... ....:::*::***:::::**::*:**:**:::::::::::::\
....:.::::**:::::::::......... ..:::::::*::::::::**:::*:*::*::::*:*:::::\
....`::::::::::::::...............::::::::::::::::***:*::::::::***::::::\
......:::::::::::::..............:::::::::::::::****:::::::::*****::.:::\
......:.::.:::.:.:........ .......:::::....::::::***:*:::::::::*.*::::::\
............:::::...................::....:..:::::*:*:::::::::::::::::::\
```.......................... ...........::::::::::*:::::.:::::::**:::::\
``.......................````  ``........:.:::::::::::::.:..:::::::::::.\
`````...... .............``````````......::::.::::::::.......::::::::...\
```.````...........````..`````````...........::.::::.......::...........\
````.```............``````````````.`   ......::::::..................`..\
```````````   ..``````````````  ``````  `.....:::::.................````\
``````` ` `````  ```````` ````` ```````....`............`........``...``\
````````````````` ````` `````````` `````.  .....```......`.........`````\
```````   ``````````````` ````````` ````````````...`````..`````.````````\
```````````````````````````````````  ```````` ```````````````````` `````\
    ```````````````  ````` `````` ````````````  ````````````````````````\
~~~~   ``````````````````    `` ` ````````      ``` `````````````` `````\
~~~~~~~~~~~~                    ````` ``` ```    ```````````````` ``` ``\
~~~~~~~~~~~~~~~~~~~~~               ````   `  ````   ````    ~~~~~~~~~~~\
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\
 ````` `    ````` ``      ```          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\
``````  ` `  ````````` ``````   `  ``` `` ```  ````   ` ``  ` ` ```` ```\
`````````..........``.`````````````````````.........`` ````` ` `````` ``\
```........................````````````................```````````....``"
  "String providing initial map topography.")

(defconst guerrilla-map-elevation-alist
  '((0 . ?~)
    (1 . ? )
    (2 . ?`)
    (3 . ?.)
    (4 . ?:)
    (5 . ?*)
    (-1 . ?\\))
  "Alist providing map elevation.
Takes the for (ELEVATION . CHAR) where ELEVATION is an integer.")

(defconst guerrilla-time-beginning
  (encode-time 56 8 7 28 5 1937)
  "Time the game begins.")

(defconst guerrilla-map-dist-ratio '(250 . 500)
  "The number of meters one coordinate point represents.")

;; Variables ===========================================================

(defvar guerrilla-time-init nil)

(defvar guerrilla-map nil)

(defvar guerrilla-map-width 72)

(defvar guerrilla-map-height
  (/ guerrilla-map-width 2))

(defvar guerrilla-map-notes nil)

(defvar guerrilla-player t)
(put 'guerrilla-player :position '(58 . 4))

(defvar guerrilla-input-marker nil)

(defvar guerrilla-map-objects
  '((bridge (29 . 32) "| |")))

;; Customizable Options ================================================

(defcustom guerrilla-buffer "*Guerrilla*"
  "Buffer name for the game."
  :type 'string
  :group 'guerrilla)

(defcustom guerrilla-map-buffer "*Guerrilla Map*"
  "Buffer name for the game's map."
  :type 'string
  :group 'guerrilla-mapping)

(defcustom guerrilla-map-player-char
  ?@
  "Character representing player position."
  :type 'character
  :group 'guerrilla-mapping)

(defcustom guerrilla-map-note-char
  ?X
  "Character representing map notes."
  :type 'character
  :group 'guerrilla-mapping)

(defcustom guerrilla-map-help
  "This is your map. Use it to mark where things are."
  "Help string to display under the map."
  :type 'string
  :group 'guerrilla-mapping)

;; Commands ============================================================

(defun guerrilla-launch-map ()
  "Open the map."
  (interactive)
  (switch-to-buffer-other-window guerrilla-map-buffer)
  (guerrilla-map-mode))

(defun guerrilla-action ()
  "Attempt to perform the submitted action."
  (interactive)
  (let ((input (buffer-substring-no-properties
                guerrilla-input-marker (point))))
    (message (s-presence input)))
  (guerrilla-message-write))

(defun guerrilla-map-get-coords ()
  "Return the coordinates at point."
  (interactive)
  (guerrilla-map-pos-to-hms (point)))

;;;###autoload
(defun guerrilla ()
  "Launch Guerrilla."
  (interactive)
  (switch-to-buffer guerrilla-buffer)
  (guerrilla-mode))

;; Distance Functions ==================================================

(defun guerrilla-map-hypot (a b)
  (sqrt (+ (* a a) (* b b))))

(defun guerrilla-map-coords-to-dist (a b)
  (let ((x (* (abs (- (car a)
                      (car b)))
              (car guerrilla-map-dist-ratio)))
        (y (* (abs (- (cdr a)
                      (cdr b)))
              (cdr guerrilla-map-dist-ratio))))
    (guerrilla-map-hypot x y)))

(defun guerrilla-map-dist-to-point ()
  (interactive)
  (let ((a (guerrilla-map-pos-to-coords (mark)))
        (b (guerrilla-map-pos-to-coords (point))))
    (if (equal a b)
        (message "The coordinates are the same.")
      (let ((dist (guerrilla-map-coords-to-dist a b)))
        (message (if (< dist 10000)
                     "About %.1f km."
                   "About %.0f km.")
                 (* dist 0.001))))))

;; Time Functions ======================================================

(defun guerrilla-time-elapsed ()
  (float-time (time-since guerrilla-time-init)))

(defun guerrilla-time-current (&optional date)
  (let ((init-time (float-time guerrilla-time-init))
        (elapsed-time (guerrilla-time-elapsed))
        (beginning-time (float-time guerrilla-time-beginning)))
    (format-time-string (if date                ; FIXME defcustom
                            "%b %-d, %-I:%M%#p"
                          "%-I:%M%#p")
                        (seconds-to-time
                         (+ (* elapsed-time 6)
                            beginning-time)))))

;; Message Functions ===================================================

(defun guerrilla-message-write (&optional erase)
  ;; (unless (= guerrilla-player-pos guerrilla-pos)
  ;;   (guerrilla-message-pos-desc))
  (let ((time (format "The time is %s." (guerrilla-time-current))))
    (with-current-buffer guerrilla-buffer
      (insert ?\n)
      (if erase
          (erase-buffer))
      (insert time ?\n)
      (insert "> ")
      ;; (goto-char (point-max))
      (setq guerrilla-input-marker (point-marker)))))

;; Map Functions =======================================================

(defun guerrilla-map-read ()
  (let (elev-list)
    (with-temp-buffer
      (insert guerrilla-map-topography)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((char (char-after))
               (elev (car (rassoc char guerrilla-map-elevation-alist)))
               (coords (guerrilla-map-pos-to-coords (point)))
               (pair (cons coords elev)))
          (setq elev-list
                (cons pair elev-list)))
        (forward-char 1)))
    (setq elev-list (reverse elev-list))
    (put 'guerrilla-map :elevation elev-list)))

(defun guerrilla-map-draw-topography ()
  (let ((elev-list (get 'guerrilla-map :elevation)))
    (goto-char (point-min))
    (dolist (pos elev-list)
      (let* ((elev (cdr pos))
             (char (cdr (assoc elev guerrilla-map-elevation-alist))))
        (insert char)))))

(defun guerrilla-map-size ()
  (let ((width guerrilla-map-width)
        (height guerrilla-map-height))
    (+ (* width height) height)))

(defun guerrilla-map-insert (s)
  (let ((n (cond ((stringp s)
                  (string-width s))
                 ((characterp s)
                  1)
                 ((error "Insertion is not a character or string")))))
    (save-excursion
      (delete-char n)
      (insert s))))

(defun guerrilla-map-pos-to-coords (pos)
  (let* ((x (% pos (1+ guerrilla-map-width)))
         (y (1+ (/ (- pos x) guerrilla-map-width))))
    (cons x y)))

(defun guerrilla-map-coords-to-pos (coords &optional screen)
  (let ((x (car coords))
        (y (cdr coords))
        (width (if screen
                   (1+ guerrilla-map-width)
                 guerrilla-map-width)))
    (+ x (* (1- y) width))))

(defun guerrilla-map-dd-to-hms (dd)
  (let* ((deg (truncate (abs dd)))
         (min (truncate (mod (* dd 60) 60)))
         (sec (truncate (mod (* dd 3600) 60))))
    (format "%s° %s' %s\"" deg min sec)))

(defun guerrilla-map-pos-to-hms (pos)
  (let* ((coords (guerrilla-map-pos-to-coords pos))
         (lat-dd (- (* (cdr coords) 0.005) 40.905))
         (long-dd (+ (* (car coords) 0.005) -4.155))
         (lat (guerrilla-map-dd-to-hms lat-dd))
         (long (guerrilla-map-dd-to-hms long-dd))
         (ns (if (< lat-dd 0) "S" "N"))
         (ew (if (< long-dd 0) "E" "W")))
      (message (format "%s%s  %s%s" lat ns long ew) t)))

(defun guerrilla-map-draw-objects ()
  (dolist (obj guerrilla-map-objects)
    (let ((pos (nth 1 obj))
          (s (nth 2 obj)))
      (goto-char (guerrilla-map-coords-to-pos pos))
      (guerrilla-map-insert s))))

(defun guerrilla-map-draw-roads (i)
  (goto-char 30)
  (dotimes (var i)
    (guerrilla-map-insert ?|)
    (if (< (+ (point) map-width) (point-max))
        (forward-char map-width))))

(defun guerrilla-map-draw-notes ()
  (dolist (note guerrilla-map-notes)
    (let ((pos (guerrilla-map-coords-to-pos (car note))))
      (goto-char pos)
      (guerrilla-map-insert guerrilla-map-note-char))))

(defun guerrilla-map-draw-player ()
  (let ((pos (guerrilla-map-coords-to-pos
              (get 'guerrilla-player :position))))
    (goto-char pos)
    (guerrilla-map-insert guerrilla-map-player-char)))

(defun guerrilla-map-redraw ()
  "Redraw the map."
  (interactive)
  (let ((mapbuf (get-buffer-create guerrilla-map-buffer)))
    (with-current-buffer mapbuf
      (let ((player-pos (guerrilla-map-coords-to-pos
                         (get 'guerrilla-player :position) t))
            (map-width guerrilla-map-width)
            (map-height guerrilla-map-height))
        (with-silent-modifications
          (deactivate-mark)
          (erase-buffer)
          (guerrilla-map-draw-topography)
          (guerrilla-map-draw-objects)
          (guerrilla-map-draw-roads map-height)
          (guerrilla-map-draw-notes)
          (guerrilla-map-draw-player)
          ;; break map into lines
          (goto-char (point-min))
          (while (not (eobp))
            (forward-char map-width)
            (insert ?\n))
          (goto-char (point-max))
          (insert ?\n guerrilla-map-help)
          (goto-char player-pos))))))

(defun guerrilla-map-annotate ()
  "Add a note to the map at point."
  (interactive)
  (let ((annotate-point
         (if (and (< (current-column) guerrilla-map-width)
                  (< (point) (guerrilla-map-size)))
             (guerrilla-map-pos-to-coords (point))
           (error "You can only add notes on the map")))
        (note
         (read-string "Note: ")))
    (push (cons annotate-point note) guerrilla-map-notes))
  (guerrilla-map-redraw))

;; Mode Keymap =========================================================

(defvar guerrilla-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'guerrilla-action)
    (define-key map (kbd "C-c C-m") 'guerrilla-launch-map)
    map)
  "Mode map for `guerrilla-mode'.")

(defvar guerrilla-map-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'push-mark-command)
    (define-key map (kbd "a") 'guerrilla-map-annotate)
    (define-key map (kbd "p") 'guerrilla-map-get-coords)
    (define-key map (kbd "d") 'guerrilla-map-dist-to-point)
    (define-key map (kbd "g") 'guerrilla-map-redraw)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Mode map for `guerrilla-map-mode'.")

;; Mode Definitions ====================================================

(define-derived-mode guerrilla-mode text-mode "Guerrilla"
  "Major mode for playing Guerrilla."
  :group 'guerrilla
  (setq guerrilla-time-init (current-time))
  (guerrilla-map-read)
  (set (make-local-variable 'scroll-step) 2)
  (insert guerrilla-splash)
  (guerrilla-message-write))

(define-derived-mode guerrilla-map-mode text-mode "Guerrilla Map"
  :group 'guerrilla-map
  (guerrilla-map-redraw)
  (setq buffer-read-only t))

(provide 'guerrilla)
;;; guerrilla.el ends here
