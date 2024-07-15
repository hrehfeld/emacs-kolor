;;; kolor.el --- Colors with lazy representation conversation -*- lexical-binding: t -*-

;; Author: Hauke Rehfeld
;; URL: https://github.com/hrehfeld/emacs-kolor
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (chroma "0.1.0"))
;; Keywords: faces extensions

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'color)
(require 'chroma)
(require 'dash)

(defconst kolor-representations '(rgb hsl rgb-string hsl-string rgb-normalized hsl-normalized name))
(defconst kolor-component-representations '(rgb hsl rgb-normalized hsl-normalized))
(defconst kolor-rgb-component-names '(red green blue))
(defconst kolor-hsl-component-names '(hue saturation lightness))
(defconst kolor-rgb-normalized-component-names '(red-normalized green-normalized blue-normalized))
(defconst kolor-hsl-normalized-component-names '(hue-normalized saturation-normalized lightness-normalized))
(defconst kolor-component-names '((red . rgb) (green . rgb) (blue . rgb)
                                  (red-normalized . rgb-normalized) (green-normalized . rgb-normalized) (blue-normalized . rgb-normalized)
                                  (hue . hsl) (saturation . hsl) (lightness . hsl)
                                  (hue-normalized . hsl-normalized) (saturation-normalized . hsl-normalized) (lightness-normalized . hsl-normalized)))
(defconst kolor-component-names-by-representation `((rgb . ,kolor-rgb-component-names)
                                                    (hsl . ,kolor-hsl-component-names)
                                                    (rgb-normalized . ,kolor-rgb-normalized-component-names)
                                                    (hsl-normalized . ,kolor-hsl-normalized-component-names)))
(defconst kolor-hsl-hue-max 100)
(defconst kolor-component-ranges `((red 0 255) (green 0 255) (blue 0 255)
                                   (red-normalized 0.0 1.0) (green-normalized 0.0 1.0) (blue-normalized 0.0 1.0)
                                   (hue 0 ,kolor-hsl-hue-max) (saturation 0 1.0) (lightness 0 1.0)
                                   (hue-normalized 0.0 1.0) (saturation-normalized 0.0 1.0) (lightness-normalized 0.0 1.0)
                                   representation   ))
(defun kolor-component-range (representation icomponent)
  "Return the valid range for the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION'."
  (cdr (nth icomponent (cdr (assoc representation kolor-component-ranges-by-representation)))))

(defun kolor-component-named-range (component-name)
  "Return the valid range for `COMPONENT-NAME'."
  (cdr (assoc component-name kolor-component-ranges)))

(defconst kolor-component-ranges-by-representation
  (cl-loop
   for (representation . component-names) in kolor-component-names-by-representation
   collect
   (cons representation
         (cl-loop
          for component-name in component-names
          collect
          (cons component-name (kolor-component-named-range component-name))))))

(defconst kolor-base-representations '(rgb hsl))
(defconst kolor-emacs-representations '(rgb-string rgb-normalized  name))

(cl-defstruct kolor
  "A color class that can represent data in different ways and converts lazily between them."
  representation
  value)

;; convert from/to emacs
(defun kolor-from-emacs (color)
  "Convert a color retrieved from Emacs (or the `color' library) into a new `KOLOR' with representation `rgb' and value `COLOR'."
  (let ((representation
         (if (stringp color)
             (cond ((string-match (chroma--anchored-regexp chroma-rgb-regexp) color) 'rgb-string)
                   (t 'name))
           'rgb-normalized)))
    (make-kolor :representation representation :value color)))

(defun kolor-to-emacs (color)
  "Convert a `COLOR' into a color that can be used by Emacs (or the `color' library)."
  (cl-check-type color kolor)
  (let ((has-emacs-representation? (memq (kolor-representation color) kolor-emacs-representations)))
    (unless has-emacs-representation?
      (setq color (kolor-ensure-representation (car kolor-emacs-representations) color)))
    (kolor-value color)))

(defun kolor-from-any (color)
  "Convert `COLOR' to `kolor' like `kolor-from-emacs', but also accepts `KOLOR' instances."
  (if (kolor-p color)
      color
    (kolor-from-emacs color)))


;; (kolor-to-emacs (make-kolor :representation 'rgb-normalized :value '(1 0 0)))
;; (kolor-to-emacs (make-kolor :representation 'rgb :value '(255 0 0)))
;; (kolor-to-emacs (make-kolor :representation 'name :value "red"))
;; (kolor-to-emacs (make-kolor :representation 'hsl-normalized :value '(0.5 0.5 0.5)))

(defun kolor-ensure-representation (to-representation color &optional copy?)
  "Convert `COLOR' to `TO-REPRESENTATION' if it is not already of that representation (unless `copy?' is non-nil)."
  (let ((from-representation (kolor-representation color)))
    (if (eq (kolor-representation color) to-representation)
        (if copy? (copy-kolor color) color)
      (let* ((from-base-representation (kolor--base-representation-chroma from-representation))
             (to-base-representation (kolor--base-representation-chroma to-representation))
             ;; transforms are defined for base-representations only
             (base-representations-identical? (eq from-base-representation to-base-representation))
             (transform (if base-representations-identical?
                            #'identity
                          (intern (format "kolor--chroma-%s-to-%s"
                                          from-base-representation
                                          to-base-representation))))
             ;; convert to and from base-representation
             (from-transform (intern (format "kolor--chroma-from-%s" from-representation)))
             (to-transform (intern (format "kolor--chroma-to-%s" to-representation))))
        (unless (fboundp transform)
          (error "No transformation from %s to %s (trying %s)" from-representation to-representation transform))

        (let* ((from-value (funcall from-transform (kolor-value color)))
               (transformed-value (funcall transform from-value))
               (to-value (funcall to-transform transformed-value)))
          (make-kolor :representation to-representation :value to-value))))))

;; (kolor-ensure-representation 'rgb (make-kolor :representation 'rgb :value '(255 128 64)))
;; (kolor-ensure-representation 'hsl (make-kolor :representation 'rgb :value '(255 128 64)))
;; (kolor-ensure-representation 'hsl-string (make-kolor :representation 'rgb :value '(255 128 64)))
:; (kolor-ensure-representation 'hsl (make-kolor :representation 'hsl-normalized :value '(0.5 0.5 0.5)))
:; (kolor-ensure-representation 'rgb (make-kolor :representation 'hsl-normalized :value '(1 0.5 0.5)))
;; (kolor-ensure-representation 'rgb (make-kolor :representation 'hsl :value '(1 0.4 0.5)))
;; (kolor-ensure-representation 'rgb-normalized (make-kolor :representation 'hsl :value '(1 0.4 0.5)))
;; (kolor-ensure-representation 'rgb (make-kolor :representation 'name :value "red"))
;; (kolor-ensure-representation 'rgb-normalized (make-kolor :representation 'name :value "red"))
;; (kolor-ensure-representation 'hsl (make-kolor :representation 'rgb :value '(255 126 0)))

(defalias 'kolor--chroma-rgb-to-hsl 'chroma-rgb-to-hsl)
(defalias 'kolor--chroma-hsl-to-rgb 'chroma-hsl-to-rgb)

(defun kolor--base-representation-chroma (representation)
  "Return the base representation of `REPRESENTATION' as used by chroma.

One of `kolor-base-representations'."
  (cond
   ((eq representation 'rgb) 'rgb)
   ((eq representation 'hsl) 'hsl)
   ((eq representation 'rgb-string) 'rgb)
   ((eq representation 'hsl-string) 'hsl)
   ((eq representation 'rgb-normalized) 'rgb)
   ((eq representation 'hsl-normalized) 'hsl)
   ((eq representation 'name) 'rgb)
   (t (error "Unknown representation %s" representation))))

;; from converts from any representation to just chroma's component wise representation
(defalias 'kolor--chroma-from-rgb #'identity)

(defalias 'kolor--chroma-from-hsl #'identity)

(defalias 'kolor--chroma-from-rgb-string #'chroma-parse-rgb)

(defalias 'kolor--chroma-from-hsl-string #'chroma-parse-hsl)

(defun kolor--chroma-from-rgb-normalized (value)
  "Convert `VALUE' from `rgb-normalized' [0.0 1.0] to `rgb' [0 255]."
  (mapcar (lambda (x) (* x 255.0))
          value))

(defun kolor--chroma-from-hsl-normalized (value)
  "Convert the hue component of `VALUE' from `hsl-normalized' [0.0 1.0] to `hsl' [0 `kolor-hsl-hue-max']."
  (list (* (nth 0 value) (float kolor-hsl-hue-max))
        (nth 1 value)
        (nth 2 value)))


(defun kolor--chroma-from-name (name)
  "Convert `NAME' from `name' to `rgb'."
  (kolor--chroma-from-rgb-normalized
   (color-name-to-rgb name)))

;; to converts from chroma's component wise representation to any representation
(defalias 'kolor--chroma-to-rgb #'identity)

(defalias 'kolor--chroma-to-hsl #'identity)

(defalias 'kolor--chroma-to-rgb-string #'chroma-format-rgb)

(defalias 'kolor--chroma-to-hsl-string #'chroma-format-hsl)

(defun kolor--chroma-to-rgb-normalized (value)
  "Convert `VALUE' from `rgb' [0 255] to `rgb-normalized' [0.0 1.0]."
  (mapcar (lambda (x) (/ x 255.0))
          value))

(defun kolor--chroma-to-hsl-normalized (value)
  "Convert the hue component of `VALUE' from `hsl' [0 `kolor-hsl-hue-max'] to `hsl-normalized' [0.0 1.0]."
  (list (/ (nth 0 value) (float kolor-hsl-hue-max))
        (nth 1 value)
        (nth 2 value)))

;; component wise access

(defun kolor-component (representation icomponent color &optional copy?)
  "Return the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (nth icomponent (kolor-value (kolor-ensure-representation representation color copy?))))

(defun kolor-set-component (representation icomponent color x &optional copy?)
  "Set the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION' to `X'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let ((color (kolor-ensure-representation representation color copy?)))
    (setf (kolor-value color)
          (-replace-at icomponent x (kolor-value color)))
    color))
;; (kolor-set-component 'rgb 0 color 128)

(defun kolor--component-named-representation (component-name)
  "Return the representation of the component named `COMPONENT-NAME'."
  (let ((representation (cdr (assoc component-name kolor-component-names))))
    (cl-assert representation t)
    representation))

;; (kolor--component-named-representation 'red)

(defun kolor--component-named-icomponent (representation component-name)
  "Return the index of the component named `COMPONENT-NAME' in `REPRESENTATION'."
  (let ((icomponent (cl-position component-name (cdr (assoc representation kolor-component-names-by-representation)))))
    (cl-assert icomponent t)
    icomponent))

;; (kolor--component-named-icomponent 'rgb 'red)
;; (kolor--component-named-icomponent 'rgb 'green)

(defun kolor-component-named (component-name color &optional copy?)
  "Return the component named `COMPONENT-NAME' of `COLOR'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let* ((representation (kolor--component-named-representation component-name)))
    (kolor-component representation (kolor--component-named-icomponent representation component-name) color copy?)))
;; (kolor-component-named 'red (make-kolor :representation 'rgb :value '(255 0 0)))
;; (kolor-component-named 'hue (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))
;; (kolor-component-named 'red (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))

(defun kolor-set-component-named (component-name color x &optional copy?)
  "Set the component named `COMPONENT-NAME' of `COLOR' to `X'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let* ((representation (kolor--component-named-representation component-name))
         (icomponent (kolor--component-named-icomponent representation component-name)))
    (kolor-set-component representation icomponent color x copy?)))

;; (defmacro kolor--define-kolor-component-accessors ()
;;   "Define accessor functions for the components of a `KOLOR' for any `kolor-component-representations'."
;;   (let ((functions
;;          (cl-loop
;;           for representation in kolor-component-representations
;;           append
;;           (let ((name (intern (format "kolor-component-%s" representation)))
;;                 (setter-name (intern (format "kolor-set-component-%s" representation))))
;;             (list
;;              `(defun ,name (color icomponent &optional copy?)
;;                 ,(format "Return the `ICOMPONENT'-th component of `COLOR' in `%s', regardless of its current representation." representation)
;;                 (kolor-component ',representation icomponent color copy?))
;;              `(defun ,setter-name (color icomponent x &optional copy?)
;;                 ,(format "Set the `ICOMPONENT'-th component of `COLOR' in `%s' to `x', regardless of its current representation.

;; Return a copy if representations need to be converted or `COPY?' is non-nil." representation)
;;                 (kolor-set-component ',representation icomponent color x copy?))
;;              `(gv-define-simple-setter ,name ,setter-name))))))
;;     `(progn
;;        ,@functions)))
;;(kolor--define-kolor-component-accessors)
;; (macroexpand-1 '(kolor--define-kolor-component-accessors))

;; (kolor-component-rgb (make-kolor :representation 'rgb :value '(255 126 125)) 1)
;; (kolor-set-component-rgb (make-kolor :representation 'rgb :value '(255 126 125)) 1 128)
;; (kolor-component-hsl-normalized (make-kolor :representation 'rgb :value '(255 127 0)) 0)
;; (setf (kolor-component-hsl-normalized (make-kolor :representation 'rgb :value '(255 127 0)) 0) 0.5)


;; (defun kolor--define-kolor-representationd-accessor (name color-name component-fn representation icomponent)

;;   `(defun ,name (color)
;;      ,(format "Return the `%s' component of `COLOR' in `%s', regardless of its current representation." color-name representation)
;;      (,component-fn color ,icomponent)))
;; (defmacro kolor--define-kolor-representationd-accessors ()

;;   (cl-loop
;;    for representation in '(rgb rgb-normalized)
;;    for color-name being the elements of kolor-rgb-component-names using (index icomponent)
;;    append
;;    (list
;;     (kolor--define-kolor-representationd-accessor (intern (format "kolor-%s" color-name))
;;                                         color-name
;;                                         'kolor-component-rgb
;;                                         representation
;;                                         (cl-position color-name '(red green blue)))))
;;   )

;; (macroexpand-1 '(kolor--define-kolor-representationd-accessors))

(defun kolor-red (color)
  "Return the red component of `COLOR', regardless of its current representation."
  (kolor-component-rgb color 0))
(defun kolor-green (color)
  "Return the green component of `COLOR', regardless of its current representation."
  (kolor-component-rgb color 1))
(defun kolor-blue (color)
  "Return the blue component of `COLOR', regardless of its current representation."
  (kolor-component-rgb color 2))

(defun kolor-hue (color)
  "Return the hue component of `COLOR', regardless of its current representation."
  (kolor-component-hsl color 0))
(defun kolor-saturation (color)
  "Return the saturation component of `COLOR', regardless of its current representation."
  (kolor-component-hsl color 1))
(defun kolor-lightness (color)
  "Return the lightness component of `COLOR', regardless of its current representation."
  (kolor-component-hsl color 2))

(defun kolor-clamp-component (representation icomponent x)
  "Clamp `X' to the valid range for the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION'."
  (let ((range (kolor-component-range representation icomponent)))
    (cl-assert range t)
    (max (nth 0 range) (min (nth 1 range) x))))

;; (kolor-clamp-component 'rgb 0 256)
;; (kolor-clamp-component 'rgb 0 -1)
;; (kolor-clamp-component 'rgb 0 128)
;; (kolor-clamp-component 'hsl 0 101)
;; (kolor-clamp-component 'hsl-normalized 0 1.1)

(defun kolor-clamp-component-named (component-name x)
  "Clamp `X' to the valid range for the component named `COMPONENT-NAME'."
  (let ((range (kolor-component-named-range component-name)))
    (cl-assert range t)
    (max (nth 0 range) (min (nth 1 range) x))))
;; (kolor-clamp-component-named 'red 256)
;; (kolor-clamp-component-named 'red -1)
;; (kolor-clamp-component-named 'red 128)
;; (kolor-clamp-component-named 'hue 101)
;; (kolor-clamp-component-named 'hue-normalized 1.1)

(defun kolor-map (target-representation fn color)
  "Convert `COLOR' to `TARGET-REPRESENTATION' and apply `FN' to it.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let* ((color (kolor-ensure-representation target-representation color 'copy?)))
    (setf (kolor-value color) (funcall fn color))))

;; (kolor-map 'hsl (lambda (color) (setf (kolor-hue color) 0.5)) (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))


(defun kolor-map-if (target-representation fn pred-fn color)
  "Convert `COLOR' to `TARGET-REPRESENTATION' and apply `FN' to it if `PRED-FN' returns non-nil when called with converted color.

Return a new `KOLOR' with the same representation as `COLOR'."
  (let* ((value (kolor-ensure-representation target-representation color))
         (color (make-kolor :representation target-representation :value value)))
    (when (funcall pred-fn color)
      (setf (kolor-value color) (funcall fn value)))
    color))

;; (kolor-map-if 'rgb (lambda (color) (kolor-value-set color 1 128)) (lambda (color) (< (kolor-red color) 200)) (make-kolor :representation 'rgb :value '(128 64 32)))


;; transform individual components
(defun kolor-transform-component (representation icomponent fn color &optional copy?)
  "Apply `FN' to the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let* ((color (kolor-ensure-representation representation color 'copy?)))
    (kolor-set-component representation icomponent color
                         (kolor-clamp-component
                          representation icomponent
                          (funcall fn
                                   (kolor-component representation icomponent color))))))
;; (kolor-transform-component 'rgb 0 (lambda (x) (* x 2)) (make-kolor :representation 'rgb :value '(128 64 32)))
;; (kolor-transform-component 'hsl 0 (lambda (x) (* x 2)) (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))



(defun kolor-transform-component-named (component-name fn color &optional copy?)
  "Apply `FN' to the component named `COMPONENT-NAME' of `COLOR'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let* ((color (kolor-ensure-representation (kolor--component-named-representation component-name) color 'copy?)))
    (kolor-set-component-named component-name color
                               (kolor-clamp-component-named component-name
                                                            (funcall fn
                                                                     (kolor-component-named component-name color))
                                                            ))))

;; (kolor-transform-component-named 'red (lambda (x) (* x 2)) (make-kolor :representation 'rgb :value '(128 64 32)))
;; (kolor-transform-component-named 'hue (lambda (x) (* x 2)) (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))

(defun kolor--funcall-centered (fn range x)
  "Call `FN' with `X''s offset against the midpoint of `RANGE'."
  (let* ((midpoint (/ (+ (nth 0 range) (nth 1 range)) 2))
         (centered-x (- x midpoint)))
    (+ midpoint (funcall fn centered-x)))
  )

(defun kolor-transform-component-centered (representation icomponent fn color &optional copy?)
  "Apply `FN' to the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION', centered around the midpoint of the valid range.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let ((range (kolor-component-range representation icomponent)))
    (kolor-transform-component representation icomponent
                               (apply-partially #'kolor--funcall-centered fn range)
                               color copy?)))

;; (kolor-transform-component-centered 'rgb-normalized 0 (lambda (x) (* x 0.5)) (make-kolor :representation 'rgb-normalized :value '(1 0.5 0.5)))


(defun kolor-transform-component-named-centered (component-name fn color &optional copy?)
  "Apply `FN' to the component named `COMPONENT-NAME' of `COLOR', centered around the midpoint of the valid range.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let ((range (kolor-component-named-range component-name)))
    (kolor-transform-component-named component-name
                                     (apply-partially #'kolor--funcall-centered fn range)
                                     color copy?)))

(defun kolor-set-comoponent-add (representation icomponent color x &optional copy?)
  "Add `X' to the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (kolor-transform-component representation icomponent (lambda (y) (+ y x)) color copy?))

;; (kolor-set-comoponent-add 'rgb 0 (make-kolor :representation 'rgb :value '(128 64 32)) 10)

(defun kolor-set-component-named-add (component-name color x &optional copy?)
  "Add `X' to the component named `COMPONENT-NAME' of `COLOR'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (kolor-transform-component-named component-name (lambda (y) (+ y x)) color copy?))


(defun kolor-set-component-multiply (representation icomponent color x &optional copy?)
  "Multiply the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION' by `X'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (kolor-transform-component representation icomponent (lambda (y) (* y x)) color copy?))

(defun kolor-set-component-named-multiply (component-name color x &optional copy?)
  "Multiply the component named `COMPONENT-NAME' of `COLOR' by `X'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (kolor-transform-component-named component-name (lambda (y) (* y x)) color copy?))

(defun kolor-set-component-multiply-centered (representation icomponent color x &optional copy?)
  "Multiply the `ICOMPONENT'-th component of `COLOR' in `REPRESENTATION' by `X', centered around the midpoint of the valid range.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (kolor-transform-component-centered representation icomponent (lambda (y) (* y x)) color copy?))

;; (kolor-set-component-multiply-centered 'rgb-normalized 0 (make-kolor :representation 'rgb-normalized :value '(1 0.5 0.5)) 0.5)

(defun kolor-set-component-named-multiply-centered (component-name color x &optional copy?)
  "Multiply the component named `COMPONENT-NAME' of `COLOR' by `X', centered around the midpoint of the valid range.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (kolor-transform-component-named-centered component-name (lambda (y) (* y x)) color copy?))

;; (kolor-set-component-named-multiply-centered 'red-normalized (make-kolor :representation 'rgb-normalized :value '(1 0 0 )) 0.5)

;; face helper functions

(defun kolor-face-attribute (face attribute &optional frame inherit)
  "Return the value of `ATTRIBUTE' for `FACE', converted to a `KOLOR'.

Do not use this function for non-color attributes."
  (let ((value (face-attribute face attribute frame inherit)))
    (when value
      (kolor-from-emacs value))))

;; (kolor-face-attribute 'default :foreground)
;; (kolor-face-attribute 'default :background)

(defun kolor-to-emacs-face-spec (args)
  "Convert a list of face attributes and values `ARGS' to a color representation and datatype that can be used by `set-face-attribute'."
  (cl-loop
   for arg being the elements of args using (index i)
   collect
   (if (keywordp arg)
       ;; just return keywords like :background
       arg
     ;; convert colors to emacs format
     (cl-check-type arg kolor)
     (kolor-to-emacs arg))))

;; (kolor-to-emacs-face-spec (list :foreground (make-kolor :representation 'rgb :value '(255 0 0)) :background (make-kolor :representation 'rgb :value '(0 255 0))))

(defun kolor-set-face-attribute (face frame &rest args)
  "Like `set-face-attribute', but accept a `KOLOR' for the color.

Do not use this function for non-color attributes."
  (apply 'set-face-attribute `(,face ,frame ,@(kolor-to-emacs-face-spec args))))

;; (kolor-set-face-attribute 'default nil :foreground (make-kolor :representation 'rgb :value '(255 0 0)))


                                        ;test
(provide 'kolor)
;;; kolor.el ends here
