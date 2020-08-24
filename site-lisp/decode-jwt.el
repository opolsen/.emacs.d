(defun add-base64-padding (encoded-string)
  (let* ((rem (mod (length encoded-string) 4))
         (pad-length (if (= rem 3) 1 rem))
         (padding (make-string pad-length ?=)))
    (concat encoded-string padding)))

(defun decode-jwt-in-region (start end)
  "Reads a JWT string from region and decodes the payload into a separate buffer"
  (interactive "r")
  (let* ((jwt-string (buffer-substring-no-properties start end))
         (jwt-payload (nth 1 (split-string jwt-string "\\.")))
         (base64-payload (add-base64-padding jwt-payload)))
    (switch-to-buffer "*decoded-jwt*")
    (json-mode)
    (insert (base64-decode-string base64-payload))
    (json-mode-beautify)))