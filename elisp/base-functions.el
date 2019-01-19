;;; -*- lexical-binding: t -*-

;; Smart split window - split and automatically jump to it
(defun split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(bind-keys ("C-x 2" . split-below-and-move)
           ("C-x 3" . split-right-and-move))

;; Kill the current buffer - good tip taken from Pragmaticemacs.
(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my/kill-this-buffer)

;; Improved kill-word
;; Found this function, as copy-word and copy-line on Uncle Daves’ .emacs.
(defun my/kill-word ()
  "Kill the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "H-d w") 'my/kill-word)

;; Improved copy-word
;; And again, the same as above but we make sure to not delete the source word.
(defun my/copy-word ()
  "Copy the whole word your cursor is in."
  (interactive)
  (save-excursion (forward-char 1)
                  (backward-word)
                  (kill-word 1)
                  (yank)))

(global-set-key (kbd "H-c w") 'my/copy-word)

;; Copy a line
;; Regardless of where your cursor is, this quickly copies a line.
(defun my/copy-line ()
  "Copies a line regardless the cursor position."
  (interactive)
  (save-excursion (kill-new
                   (buffer-substring
                    (point-at-bol)
                    (point-at-eol)))))

(global-set-key (kbd "H-c l") 'my/copy-line)

;; Org header IDs
(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring,
if no ID is there then create a new unique ID.
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
       (interactive)
       (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
     (setq mytmpid (funcall 'org-id-get-create))
     (kill-new mytmpid)
     (message "Copied %s to killring (clipboard)" mytmpid)
       ))

(global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)

;; Create parent directories when saving file
;; Copied from sensible-defaults.el.
(defun sensible-defaults/offer-to-create-parent-directories-on-save ()
  "When saving a file in a directory that doesn't exist, offer
to (recursively) create the file's parent directories."
  (add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t)))))))


(provide 'base-functions)
;;; base-functions.el ends here
