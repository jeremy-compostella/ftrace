;;; gplot.el --- Plot data module

;; Copyright (C) 2018 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 1.0
;; Keywords:

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

(require 'gnuplot)

(defcustom gplot-terminal "wxt"
  "Default terminal.")

(defcustom gplot-multiplot-max-row 4
  "Max row before using columns.")

(defvar gplot-term -1)
(defvar gplot-multiplot nil
  "t when currently plot in multiplot mode.")

(defcustom gplot-palette
  '("#324E72" "#6A3A4C" "#000000" "#1CE6FF" "#FF34FF" "#FF4A46" "#008941"
    "#006FA6" "#A30059" "#FFDBE5" "#7A4900" "#0000A6" "#63FFAC" "#B79762"
    "#004D43" "#8FB0FF" "#997D87" "#5A0007" "#809693" "#FEFFE6" "#1B4400"
    "#4FC601" "#3B5DFF" "#4A3B53" "#FF2F80" "#61615A" "#BA0900" "#6B7900"
    "#00C2A0" "#FFAA92" "#FF90C9" "#B903AA" "#D16100" "#DDEFFF" "#000035"
    "#7B4F4B" "#A1C299" "#300018" "#0AA6D8" "#013349" "#00846F" "#372101"
    "#FFB500" "#C2FFED" "#A079BF" "#CC0744" "#C0B9B2" "#C2FF99" "#001E09"
    "#00489C" "#6F0062" "#0CBD66" "#EEC3FF" "#456D75" "#B77B68" "#7A87A1"
    "#788D66" "#885578" "#FAD09F" "#FF8A9A" "#D157A0" "#BEC459" "#456648"
    "#0086ED" "#886F4C" "#34362D" "#B4A8BD" "#00A6AA" "#452C2C" "#636375"
    "#A3C8C9" "#FF913F" "#938A81" "#575329" "#00FECF" "#B05B6F" "#8CD0FF"
    "#3B9700" "#04F757" "#C8A1A1" "#1E6E00" "#7900D7" "#A77500" "#6367A9"
    "#A05837" "#6B002C" "#772600" "#D790FF" "#9B9700" "#549E79" "#FFF69F"
    "#201625" "#72418F" "#BC23FF" "#99ADC0" "#3A2465" "#922329" "#5B4534"
    "#FDE8DC" "#404E55" "#0089A3" "#CB7E98" "#A4E804")
  "Palette of color to use.")

(defsubst gplot-escape (str)
  (replace-regexp-in-string "_" "\\_" str t t))

(defun gplot-exec (&rest cmds)
  (with-temp-buffer
    (insert (mapconcat 'identity cmds "\n"))
    (gnuplot-mode)
    (save-window-excursion
      (gnuplot-send-buffer-to-gnuplot))))

(defun gplot (title xlabel ylabel plot-cmd)
  (gplot-exec
      (if gplot-multiplot
	  ""
	(format "reset\nset term %s %d" gplot-terminal (incf gplot-term)))
       (format "set title '%s'" (gplot-escape title))
       (format "set xlabel '%s'" (gplot-escape xlabel))
       (format "set ylabel '%s'" (gplot-escape ylabel))
       "set grid"
       "set datafile separator \"\\t\""
       "set key invert reverse Left outside"
       "set style fill solid border -1"
       (concat "plot " plot-cmd)))

(defmacro with-multiplot (nb title &rest body)
  (declare (indent 2))
  `(progn
     (gplot-exec "reset"
       (format "set term qt %d" (incf gplot-term))
       (let ((row ,nb)
	     (col 1))
	 (when (> ,nb gplot-multiplot-max-row)
	   (setq col (round (sqrt ,nb)))
	   (setq row (round (fceiling (/ ,nb (sqrt ,nb))))))
	 (format "set multiplot layout %d, %d  font \",12\" %s\n"
		 row col (concat "title '" (gplot-escape ,title) "'"))))
     (let ((gplot-multiplot t))
       ,@body)
     (gplot-exec "unset multiplot")))

(defun gplot-cumulative (title xlabel ylabel data titles)
  (dolist (line (cdr data))
    (let ((cur (cdr line)))
      (while (cdr cur)
	(setcar (cdr cur) (+ (cadr cur) (car cur)))
	(setq cur (cdr cur))))
    (setf (cdr line) (nreverse (cdr line))))
  (setq titles (nreverse titles))
  (let ((file (make-temp-file "gplot"))
	(i 1))
    (with-temp-buffer
      (dolist (line (cdr data))
	(insert (mapconcat (curry 'format "%f") line "\t") "\n"))
      (write-file file))
    (setf titles (mapcar 'gplot-escape titles))
    (cl-flet ((format-line (x)
	        (format "'%s' using 1:%d with filledcurve x1 lc rgbcolor '%s' title '%s'"
			file (incf i)
			(nth (% i (length gplot-palette)) gplot-palette) x)))
      (gplot title xlabel ylabel (mapconcat #'format-line titles ",\\\n")))))

(defun gplot-plot (title xlabel ylabel data metadata)
  (cl-flet ((store-data (x)
	     (with-temp-buffer
	       (let ((file (make-temp-file "gplot")))
		 (dolist (line x)
		   (insert (mapconcat (curry 'format "%f") line "\t") "\n"))
		 (write-file file)
		 file))))
    (let ((idata (mapcar* 'list (mapcar #'store-data data) metadata))
	  (i 1))
      (cl-flet ((format-line (x)
		  (format "'%s' using 1:2 with %s lc rgbcolor '%s' title '%s'"
			  (car x) (assoc-default 'style (cadr x))
			  (nth (% (incf i) (length gplot-palette)) gplot-palette)
			  (gplot-escape (assoc-default 'title (cadr x))))))
	(gplot title xlabel ylabel (mapconcat #'format-line idata ",\\\n"))))))

(provide 'gplot)
