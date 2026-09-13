;;; test-escapes.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for OSC-aware escape handling (zones, cells) and small fixes

(in-package #:tuition-tests)

(def-suite escape-fix-tests
  :description "OSC handling in zone-scan and the cells parser, plus
keybindings-help separator handling."
  :in tuition-tests)

(in-suite escape-fix-tests)

(defun %osc8 (uri text)
  "Wrap TEXT in an OSC 8 hyperlink to URI (ST-terminated)."
  (format nil "~C]8;;~A~C\\~A~C]8;;~C\\"
          #\Escape uri #\Escape text #\Escape #\Escape))

;;; --- zone-scan (cl-tuition-lsq) ---

(test zone-scan-ignores-osc-hyperlinks
  "OSC 8 payload is not counted as visible columns in zone positions."
  (let* ((mgr (make-zone-manager))
         (marked (zone-mark "btn" "CLICK" mgr))
         (text (concatenate 'string
                            (%osc8 "https://example.com" "link")
                            " " marked)))
    (zone-scan text mgr)
    (let ((zone (zone-get "btn" mgr)))
      (is (not (null zone)))
      ;; "link" is 4 visible columns plus a space: the zone starts at 5.
      (is (= 5 (zone-info-start-x zone)))
      (is (= 9 (zone-info-end-x zone))))))

(test zone-scan-preserves-osc-sequences
  "Non-marker escapes pass through zone-scan intact."
  (let* ((mgr (make-zone-manager))
         (text (%osc8 "https://example.com" "link"))
         (cleaned (zone-scan text mgr)))
    (is (string= text cleaned))))

;;; --- cells parser (cl-tuition-p1b) ---

(test cells-parser-captures-hyperlink
  "The cells parser records OSC 8 URIs into cell-link, and an empty URI
ends the link."
  (let* ((str (concatenate 'string "a" (%osc8 "https://x.io" "bc") "d"))
         (buf (parse-styled-string str 10 1)))
    (is (null (cell-link (screen-buffer-ref buf 0 0))))
    (is (equal "https://x.io" (cell-link (screen-buffer-ref buf 1 0))))
    (is (equal "https://x.io" (cell-link (screen-buffer-ref buf 2 0))))
    (is (null (cell-link (screen-buffer-ref buf 3 0))))))

(test cells-parser-hyperlink-with-bel-terminator
  "OSC 8 sequences terminated by BEL are captured too."
  (let* ((str (format nil "~C]8;;https://y.io~Cz~C]8;;~C"
                      #\Escape #\Bel #\Escape #\Bel))
         (buf (parse-styled-string str 5 1)))
    (is (equal "https://y.io" (cell-link (screen-buffer-ref buf 0 0))))))

(test cells-parser-skips-other-osc
  "A non-hyperlink OSC sequence neither shifts the cursor nor sets a link."
  (let* ((str (format nil "~C]0;window title~C\\ab" #\Escape #\Escape))
         (buf (parse-styled-string str 5 1)))
    (is (char= #\a (cell-char (screen-buffer-ref buf 0 0))))
    (is (null (cell-link (screen-buffer-ref buf 0 0))))
    (is (char= #\b (cell-char (screen-buffer-ref buf 1 0))))))

;;; --- keybindings-help separator (cl-tuition-cx7) ---

(test keybindings-help-uses-separator
  "The :separator argument goes between entries."
  (let ((bindings (list (make-keybinding :keys '(#\j) :help-key "j"
                                         :help-desc "down")
                        (make-keybinding :keys '(#\k) :help-key "k"
                                         :help-desc "up")
                        (make-keybinding :keys '(#\q) :help-key "q"
                                         :help-desc "quit"))))
    (is (string= "j down • k up • q quit"
                 (keybindings-help bindings :separator " • ")))
    (is (string= "j down  k up  q quit"
                 (keybindings-help bindings)))))

;;; --- truncate-text exact fit (cl-tuition-erp) ---

(test truncate-exact-fit-keeps-text
  "Text exactly at width is returned unchanged with the default ellipsis."
  (is (string= "hello" (truncate-text "hello" 5)))
  (is (string= "hello" (ellipsize "hello" 5))))

;;; --- clipboard/query commands (cl-tuition-tcs) ---

(test clipboard-commands-produce-escape-messages
  "The clipboard and color-query commands yield write-escape messages
carrying the right OSC sequences."
  (let ((msg (funcall (set-clipboard-cmd "hi"))))
    (is (write-escape-msg-p msg))
    (is (search "]52;c;" (write-escape-msg-sequence msg)))
    (is (search (cl-base64:string-to-base64-string "hi")
                (write-escape-msg-sequence msg))))
  (is (search "]52;c;?" (write-escape-msg-sequence
                         (funcall (read-clipboard-cmd)))))
  (is (search "]52;p;" (write-escape-msg-sequence
                        (funcall (set-primary-clipboard-cmd "x")))))
  (is (search "]11;?" (write-escape-msg-sequence
                       (funcall (request-background-color-cmd)))))
  (is (search "]10;?" (write-escape-msg-sequence
                       (funcall (request-foreground-color-cmd)))))
  (is (search "]12;?" (write-escape-msg-sequence
                       (funcall (request-cursor-color-cmd))))))
