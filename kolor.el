;;; kolor.el --- Colors with lazy representation conversation -*- lexical-binding: t -*-

;; Author: Hauke Rehfeld
;; URL: https://github.com/hrehfeld/emacs-kolor
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (chroma "0.1.0") (compat))
;; Keywords: faces extensions

;;; Commentary:
;;
;; Convert and modify colors with automatic color space conversions.

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'color)
(require 'chroma)
(require 'dash)


(defconst kolor-component-names-by-representation `((rgb . (red green blue))
                                                    (hsl . (hue saturation lightness))
                                                    (rgb-normalized . (red-normalized green-normalized blue-normalized))
                                                    (hsl-normalized . (hue-normalized saturation-normalized lightness-normalized))
                                                    (cie-lab . (cie-l cie-a cie-b))))
(defconst kolor-component-representations (cl-loop for (representation . component-names) in kolor-component-names-by-representation collect representation))
(defconst kolor-representations (append kolor-component-representations '( rgb-string hsl-string name)))

(defconst kolor-base-representations-alist
  '((rgb . rgb)
    (hsl . hsl)
    (rgb-normalized . rgb)
    (hsl-normalized . hsl)
    (cie-lab . cie-lab)
    (rgb-string . rgb)
    (hsl-string . hsl)
    (name . rgb)))

(defconst kolor-component-names
  (cl-loop
   for (representation . component-names) in kolor-component-names-by-representation
   append
   (cl-loop
    for component-name in component-names
    collect
    (cons component-name representation))))

(defconst kolor-component-types-by-representation `((rgb integer integer integer)
                                                    (hsl integer float float)
                                                    (cie-lab integer integer integer)
                                                    (rgb-normalized float float float)
                                                    (hsl-normalized float float float)
                                                    (rgb-string string)
                                                    (hsl-string string)
                                                    (name string)))

(defconst kolor-uncheckable-representations '(name))

(defconst kolor-hsl-hue-max 359)
(defconst kolor-component-ranges `((red 0 255) (green 0 255) (blue 0 255)
                                   (red-normalized 0.0 1.0) (green-normalized 0.0 1.0) (blue-normalized 0.0 1.0)
                                   (hue 0 ,kolor-hsl-hue-max) (saturation 0 1.0) (lightness 0 1.0)
                                   (hue-normalized 0.0 1.0) (saturation-normalized 0.0 1.0) (lightness-normalized 0.0 1.0)
                                   (cie-l 0 100) (cie-a -128 127) (cie-b -128 127)))

(defconst kolor-component-clamp-method-by-base-representation
  `((rgb clamp clamp clamp)
    (hsl rotate clamp clamp)
    (cie-lab clamp clamp clamp)))

(defconst kolor-clamp-functions '((clamp . kolor--clamp) (rotate . kolor--rotate)))

(defconst kolor-type-constructors '((integer . truncate) (float . float) (string . identity)))

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

(defconst kolor-base-representations '(rgb hsl cie-lab))
(defconst kolor-emacs-representations '(rgb-string name))

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

(defun kolor-is-emacs? (color)
  "Return color if `COLOR' is a color."
  (when (and (stringp color)
             (color-defined-p color))
    color))

;; (kolor-to-emacs (make-kolor :representation 'rgb-normalized :value '(1 0 0)))
;; (kolor-to-emacs (make-kolor :representation 'rgb :value '(255 0 0)))
;; (kolor-to-emacs (make-kolor :representation 'name :value "red"))
;; (kolor-to-emacs (make-kolor :representation 'hsl-normalized :value '(0.5 0.5 0.5)))


(defun kolor--valuep (value)
  "Return non-nil if `VALUE' is a valid color value to be used as :value in `make-kolor'."
  (or (and (listp value)
                  (--all-p (or (integerp it) (floatp it))
                           value))
             (stringp value)))
;; (kolor--valuep '(1 0 0))
;; (kolor--valuep '(1 0 0.0))
;; (kolor--valuep '(1 0 "0"))
;; (kolor--valuep 0)

(defun kolor--wrap-value-fn (fn color &optional copy? &rest args)
  "Call `FN' with `(representation value . ARGS)' of `COLOR'.

Modify `COLOR' in place and return that unless `COPY?' is non-nil, then return a copy."
  (let ((color (if copy? (copy-kolor color) color)))
    (setf (kolor-value color)
          (apply fn (cons (kolor-representation color) (cons (kolor-value color) args))))
    (cl-check-type (kolor-value color) kolor--value)
    color))


(defun kolor--transform-fn (kind representation)
  "Return the transformation function to base representation for `KIND' and `REPRESENTATION'.

`KIND' is one of `from' or `to'."
  (cl-assert (memq kind '(from to)) t)
  (if (memq representation kolor-base-representations)
      #'identity
    (intern (format "kolor--chroma-%s-%s" kind representation))))

(defun kolor--base-transform-fn (from-base-representation to-base-representation)
  "Return the transformation function from `FROM-BASE-REPRESENTATION' to `TO-BASE-REPRESENTATION'."
  (cl-assert (memq from-base-representation kolor-base-representations) t)
  (cl-assert (memq to-base-representation kolor-base-representations) t)
  (if (eq from-base-representation to-base-representation)
      #'identity
    (intern (format "kolor--chroma-%s-to-%s" from-base-representation to-base-representation))))


(defun kolor--from-to-base-representation (kind representation value)
  "Convert `VALUE' from `REPRESENTATION' to its base representation where `KIND' is either `from' or `to'."
  (let ((transform (kolor--transform-fn kind representation)))
        (unless (fboundp transform)
          (error "No transformation %s: %s %s %s" transform representation kind (kolor--base-representation-chroma representation)))
        ;;(message "Transforming %s %s %S with %S" kind representation value transform)
        (funcall transform value)))

;; (kolor--from-to-base-representation 'from 'rgb '(255 128 64))
;; (kolor--from-to-base-representation 'from 'rgb-normalized '(1 0.5 0.25))
;; (kolor--from-to-base-representation 'from 'hsl-normalized '(0.5 0.5 0.5))


(defun kolor--ensure-representation (from-representation value to-representation)
  "Convert `VALUE' from `FROM-REPRESENTATION' to `TO-REPRESENTATION'."
  (cl-check-type value kolor--value)
  (if (eq from-representation to-representation)
      value
    (let* ((from-base-representation (kolor--base-representation-chroma from-representation))
           (to-base-representation (kolor--base-representation-chroma to-representation))
           ;; transforms are defined for base-representations only
           (transform (kolor--base-transform-fn from-base-representation to-base-representation))
           ;; convert to and from base-representation
           (from-transform (kolor--transform-fn 'from from-representation))
           (to-transform (kolor--transform-fn 'to to-representation)))
      (unless (fboundp transform)
        (error "No transformation from %s to %s (trying %s)" from-representation to-representation transform))

      (let* ((to-value (-some->> value
                         (kolor--ensure-type from-representation)
                         (funcall from-transform)
                         (kolor--ensure-valid from-base-representation)
                         (funcall transform)
                         (kolor--ensure-range to-base-representation)
                         (funcall to-transform)
                         (kolor--ensure-type to-representation))))
        (cl-assert to-value t "Transform: (%S %S %S)" from-transform transform to-transform)
        to-value))))

(defun kolor-ensure-representation (to-representation color &optional copy?)
  "Convert `COLOR' to `TO-REPRESENTATION' if it is not already of that representation (unless `copy?' is non-nil)."
  (cl-check-type color kolor)
  (let* ((color (if copy? (copy-kolor color) color))
         (from-representation (kolor-representation color)))
    (setf (kolor-representation color) to-representation
          (kolor-value color)
          (kolor--ensure-representation from-representation (kolor-value color) to-representation))
    color))

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
;; (kolor-ensure-representation 'rgb-string (make-kolor :representation 'hsl-normalized :value '(1.0 1.0 0.9)))
;; test lab
;; (kolor-ensure-representation 'cie-lab (make-kolor :representation 'rgb :value '(255 255 255)))

;; type conversion and clamping/rotating
(defun kolor--ensure-type (representation value)
  "Ensure that `VALUE' is of the correct type for `REPRESENTATION'."
  (cl-check-type value kolor--value)
  (if  (memq representation kolor-uncheckable-representations)
      value
    (let ((types (cdr (assoc representation kolor-component-types-by-representation))))
      (cl-assert types t)
      ;; if types has only a single type, we assume that the value is a single component like a hex string
      ;; and subsequently convert back and forth
      (let ((is-multi-component? (length> types 1)))
        (unless is-multi-component?
          (setq value (kolor--from-to-base-representation 'from representation value)
                types (cdr (assoc (kolor--base-representation-chroma representation) kolor-component-types-by-representation)))
          (cl-assert types t))
        (cl-assert (length= types (length value)) t)
        ;;(message "kolor--ensure-type %S %S" types value)
        (let ((value (cl-loop for type in types
                              for x in value
                              collect
                              (let ((constructor (cdr (assoc type kolor-type-constructors))))
                                (cl-assert constructor t)
                                (funcall constructor x))
                              )))
          (cl-assert (length= types (length value)) t)
          (if is-multi-component?
              value
            (kolor--from-to-base-representation 'to representation value)))))))
;; (kolor--ensure-type 'rgb '(0 0 0))
;; (kolor--ensure-type 'rgb '(0.0 0.0 0.0))
;; (kolor--ensure-type 'hsl '(0 0 0.0))
;; (kolor--ensure-type 'hsl '(0 0 0))
;; (kolor--ensure-type 'rgb-string "#fff000")
;; (kolor--ensure-type 'rgb-string "#fff000")
;; (kolor--ensure-type 'name "ForestGreen")



(defun kolor--ensure-range (representation value)
  "Ensure that `VALUE' is in the valid range for `REPRESENTATION' by either clamping or rotating."
  (if  (memq representation kolor-uncheckable-representations)
      value
    (let* ((base-representation (kolor--base-representation-chroma representation))
           (clamp-methods (cdr (assoc base-representation kolor-component-clamp-method-by-base-representation)))
           (ranges (cdr (assoc representation kolor-component-ranges-by-representation))))
      (when (length= ranges 1)
        (setq value (kolo)))
      (cl-assert clamp-methods t)
      (cl-assert ranges t)
      (cl-assert (= (length value) (length ranges) (length clamp-methods)) t)
      (cl-loop for clamp-method in clamp-methods
               for range in ranges
               for x in value
               collect
               (let ((wrap-fn (cdr (assoc clamp-method kolor-clamp-functions)))
                     (range (cdr range)))
                 ;;(message "clamp-method: %S range: %S x: %S" clamp-method range x)
                 (apply wrap-fn (cons x range)))))))
;; (kolor--ensure-range 'rgb '(0 0 0))
;; (kolor--ensure-range 'rgb '(0 0 256))
;; (kolor--ensure-range 'rgb '(0 0 -1))
;; (kolor--ensure-range 'hsl '(0 0 0))
;; (kolor--ensure-range 'hsl '(0 0 1.1))
;; (kolor--ensure-range 'hsl '(0 0 -0.1))
;; (kolor--ensure-range 'hsl '(359 0.5 0.5))
;; (kolor--ensure-range 'hsl '(360 0.5 0.5))
;; (kolor--ensure-range 'hsl '(-361 0.5 0.5))

(defun kolor--ensure-valid (representation value)
  "Ensure that `VALUE' is valid for `REPRESENTATION'."
  (->> value
       (kolor--ensure-type representation)
       (kolor--ensure-range representation)))
;; (kolor--ensure-valid 'rgb '(0 0 0))
;; (kolor--ensure-valid 'rgb '(0 0 256))
;; (kolor--ensure-valid 'rgb '(0 0 -1))
;; (kolor--ensure-valid 'hsl '(0 0 0))
;; (kolor--ensure-valid 'hsl '(0 0 1.1))
;; (kolor--ensure-valid 'hsl '(0 0 -0.1))
;; (kolor--ensure-valid 'hsl '(359 0.5 0.5))
;; (kolor--ensure-valid 'hsl '(360 0.5 0.5))
;; (kolor--ensure-valid 'hsl '(-361 0.5 0.5))


(defun kolor-ensure-range (color &optional copy?)
  "Ensure that the value of `COLOR' is in the valid range for its representation.

Modify `COLOR' in place and return that unless `COPY?' is non-nil, then return a copy."
  (kolor--wrap-value-fn #'kolor--ensure-range color copy?))

(defun kolor-ensure-type (color &optional copy?)
  "Ensure that the value of `COLOR' is of the correct type for its representation.

Modify `COLOR' in place and return that unless `COPY?' is non-nil, then return a copy."
  (kolor--wrap-value-fn #'kolor--ensure-type color copy?))

(defun kolor-ensure-valid (color &optional copy?)
  "Ensure that the value of `COLOR' is of the correct type and range for its representation.

Modify `COLOR' in place and return that unless `COPY?' is non-nil, then return a copy."
  (kolor--wrap-value-fn #'kolor--ensure-valid color copy?))

(defalias 'kolor--chroma-rgb-to-hsl 'chroma-rgb-to-hsl)
(defalias 'kolor--chroma-hsl-to-rgb 'chroma-hsl-to-rgb)

(defun kolor--chroma-rgb-to-cie-lab (value)
  (let ((value (kolor--chroma-to-rgb-normalized value)))
    (apply #'color-srgb-to-lab value)))
(defun kolor--chroma-cie-lab-to-rgb (value)
  (kolor--chroma-from-rgb-normalized (apply #'color-lab-to-srgb value)))

(defun kolor--chroma-hsl-to-cie-lab (value)
  "Convert `VALUE' from `hsl' to `cie-lab'."
  (kolor--chroma-rgb-to-cie-lab (kolor--chroma-hsl-to-rgb value)))
(defun kolor--chroma-cie-lab-to-hsl (value)
  "Convert `VALUE' from `cie-lab' to `hsl'."
  (kolor--chroma-rgb-to-hsl (kolor--chroma-cie-lab-to-rgb value)))

(defun kolor--base-representation-chroma (representation)
  "Return the base representation of `REPRESENTATION' as used by chroma.

One of `kolor-base-representations'."
  (let ((base-representation (alist-get representation kolor-base-representations-alist)))
    (cond
     (base-representation)
     (t (error "Unknown representation %s" representation)))
    ))

;; from converts from any representation to just chroma's component wise representation
(defalias 'kolor--chroma-from-rgb #'identity)

(defalias 'kolor--chroma-from-hsl #'identity)

(defalias 'kolor--chroma-from-rgb-string #'chroma-parse-rgb)

(defalias 'kolor--chroma-from-hsl-string #'chroma-parse-hsl)

(defun kolor--chroma-from-rgb-normalized (value)
  "Convert `VALUE' from `rgb-normalized' [0.0 1.0] to `rgb' [0 255]."
  (mapcar (lambda (x) (truncate (* x 255.0)))
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


(defun kolor--clamp (value value-min value-max)
  "Clamp `VALUE' to the range [`VALUE-MIN' `VALUE-MAX']."
  (max value-min (min value-max value)))

(defun kolor--rotate (value value-min value-max)
  "Rotate `VALUE' to the range [`VALUE-MIN' `VALUE-MAX') by wrapping around."
  (-> value
      (- value-min)
      (mod (- value-max value-min))
      (+ value-min)))
;; (kolor--rotate 140 -50 0)
;; (kolor--rotate 140 0 360)
;; (kolor--rotate 140 0 100)
;; (kolor--rotate 361 0 360)
;; (kolor--rotate 360 0 359)

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
    (kolor-ensure-valid (kolor-set-component representation icomponent color
                         (funcall fn
                                  (kolor-component representation icomponent color))))))
;; (kolor-transform-component 'rgb 0 (lambda (x) (* x 2)) (make-kolor :representation 'rgb :value '(128 64 32)))
;; (kolor-transform-component 'hsl 0 (lambda (x) (* x 2)) (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))



(defun kolor-transform-component-named (component-name fn color &optional copy?)
  "Apply `FN' to the component named `COMPONENT-NAME' of `COLOR'.

Return a copy if representations need to be converted or `COPY?' is non-nil."
  (let* ((color (kolor-ensure-representation (kolor--component-named-representation component-name) color 'copy?)))
    (kolor-ensure-valid (kolor-set-component-named component-name color
                               (funcall fn
                                        (kolor-component-named component-name color))
                               ))))

;; (kolor-transform-component-named 'red (lambda (x) (* x 2)) (make-kolor :representation 'rgb :value '(128 64 32)))
;; (kolor-transform-component-named 'hue (lambda (x) (* x 2)) (make-kolor :representation 'hsl :value '(0.1 0.2 0.3)))

(defun kolor--midpoint (range)
  "Return the midpoint of `RANGE'."
  (/ (+ (nth 0 range) (nth 1 range)) 2.0))

(defun kolor--funcall-centered (fn range x)
  "Call `FN' with `X''s offset against the midpoint of `RANGE'."
  (let* ((midpoint (kolor--midpoint range))
         (centered-x (- x midpoint)))
    (+ midpoint (funcall fn centered-x)))
  )

;; (kolor--funcall-centered (lambda (x) (* x 2)) '(0 255) 128)
;; (kolor--funcall-centered (lambda (x) (* x -1)) '(0 1) 0.5)

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

Do not use this function for non-color attributes, it will return nil."
  (let ((value (face-attribute face attribute frame inherit)))
    (when (kolor-is-emacs? value)
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
     (if (kolor-p arg)
         (kolor-to-emacs arg)
       arg))))

;; (kolor-to-emacs-face-spec (list :foreground (make-kolor :representation 'rgb :value '(255 0 0)) :background (make-kolor :representation 'rgb :value '(0 255 0))))

(defun kolor-set-face-attribute (face frame &rest args)
  "Like `set-face-attribute', but accept a `KOLOR' for the color.

Do not use this function for non-color attributes."
  (apply 'set-face-attribute `(,face ,frame ,@(kolor-to-emacs-face-spec args))))

;; (kolor-set-face-attribute 'default nil :foreground (make-kolor :representation 'rgb :value '(255 0 0)))


(defvar kolor--make-face-light-dark-attributes-alist
  nil
  "Alist of `(FACE . SPEC)' as seen when first touching face.")

(defun kolor-make-face-light-dark (face &optional lightness-component-name)
  "Modify `FACE' to create a light and a dark variant where one is the lightness-inverted equivalent of the other."
  (let* ((lightness-component-name (or lightness-component-name 'lightness-normalized))
         (representation (alist-get lightness-component-name kolor-component-names))
         (previous-spec (face-all-attributes face))
         last-keyword
         spec
         dark-spec
         light-spec)
    ;;(message "previous-spec: %S" previous-spec)
    (cl-loop for (attribute . value) in previous-spec
             do
             (if (kolor-is-emacs? value)
                 (let* ((value (kolor-ensure-representation representation (kolor-from-emacs value)))
                        (background-attribute? (eq attribute :background))
                        (is-light? (> (kolor-component-named lightness-component-name value)
                                      (kolor--midpoint (kolor-component-named-range lightness-component-name))))

                        (opposite (kolor-set-component-named-multiply-centered lightness-component-name value -1.0))
                        (light-color (if is-light? value opposite))
                        (dark-color (if is-light? opposite value)))
                   ;; (apply #'message "%S %S" (mapcar (lambda (c) (kolor-ensure-representation 'hsl c)) (list value opposite)))
                   ;; (apply #'message "%S %S" (mapcar (lambda (c) (kolor-ensure-representation 'rgb-normalized c)) (list value opposite)))
                   (if background-attribute?
                       (let ((light-org light-color))
                         (setq light-color dark-color)
                         (setq dark-color light-org)))
                   (push attribute light-spec)
                   (push dark-color light-spec) 
                   (push attribute dark-spec)
                   (push light-color dark-spec))
               ;; (push attribute spec)
               ;; (push value spec)
               ))
    (when (or light-spec dark-spec)
      (let* ((spec `((default . ,previous-spec)
                     (((background dark)) . ,(kolor-to-emacs-face-spec (nreverse dark-spec)))
                     (t ,(kolor-to-emacs-face-spec (nreverse light-spec))))))
        ;;(message "kolor-make-face-light-dark: %S spec: %S previous-spec: %S" face spec previous-spec)
        (face-spec-set face nil 'reset)
        (face-spec-set face spec 'face-defface-spec)
        spec))))
;; (kolor-make-face-light-dark 'default 'cie-l)

(defun kolor-face-list-light-dark (&optional face-list)
  (interactive)
  "Apply `kolor-make-face-light-dark' to all faces in `FACE-LIST' or all faces, see `(face-list)'."
  (cl-loop for face in (or face-list (face-list))
           do
           (kolor-make-face-light-dark face 'lightness)))

                                        ;test
(provide 'kolor)
;;; kolor.el ends here
