;;; keys.el --- For keybindings that I didn’t know where else to put -*- lexical-binding: t; -*-
;;
;;
;; Copyright (C) 2021 Alec Stewart

;; Author: Alec Stewart <alec-stewart@protonmail.com>
;; URL: https://github.com/alecStewart1/dotemacs
;; Keywords: emacs .emacs.d dotemacs

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;
;;; Commentary:
;;
;;  This is something that I might not use if I use Evil or some other modal
;;  editing package.
;;
;;; Code:

(require 'lib)
(require 'general)

;;; The Emacs "Leader" Key and Keymap
;;;

(unless (and (package-installed-p 'evil)
             (bound-and-true-p evil-mode))
  (defvar ctrl-c-map (make-sparse-keymap "CTRL-C Keymap")
    "The keymap for generic key combinations prefixed with Ctrl-c.")

  (general-create-definer emacs:leader-def
    :prefix "C-c"
    :keymap 'ctrl-c-map))



(provide 'keys)
;;; keys.el ends here
