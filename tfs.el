;;; tfs.el --- MS Team Foundation Server commands for Emacs.
;; Authors    : Dino Chiesa <dpchiesa@outlook.com>, Sebastian Monia <smonia@outlook.com>
;; Version    : 2018.5.11
;;; Commentary:
;;
;; Basic steps to setup:
;;   1. Obtain the Team Explorer Everywhere CLI tool from https://github.com/Microsoft/team-explorer-everywhere/releases
;;   2. Place `tfs.el' in your `load-path'.
;;   3. In your .emacs file:
;;        (require 'tfs)
;;        (setq tfs-cmd  "location/of/TEE/tf")
;;        (setq tfs-login "/login:domain\\userid,password")
;;              -or-
;;        (setq tfs-login (getenv "TFSLOGIN"))
;;   4. also in your .emacs file:
;;        set local or global key bindings for tfs commands.  Example:
;;
;;        (global-set-key  "\C-ctp" 'tfs-pendingchanges)
;;        (global-set-key  "\C-cto" 'tfs-checkout)
;;        (global-set-key  "\C-cti" 'tfs-checkin)
;;        (global-set-key  "\C-ctr" 'tfs-rename)
;;        (global-set-key  "\C-ctg" 'tfs-get)
;;        (global-set-key  "\C-cth" 'tfs-history)
;;        (global-set-key  "\C-ctc" 'tfs-changeset)
;;        (global-set-key  "\C-ctu" 'tfs-undo)
;;        (global-set-key  "\C-ct-" 'tfs-delete)
;;        (global-set-key  "\C-ct+" 'tfs-add)
;;; Code:

(require 'dom)
(require 'tablist)

(defcustom tfs-cmd  "C:/HomeFolder/TEE-CLC-14.134.0/tf.cmd"
  "Location of the 'Team Explorer Everywhere' command line tool."
  :type 'string
  :group 'tfs)

(defcustom tfs-login ""
  "Values for the -login option.  Ignored if empty."
  :type 'string
  :group 'tfs)

(defcustom tfs-collection-url ""
  "URL of the TFS Collection.  If empty, the TEE CLI will assume the collection from the existing folder mappings."
  :type 'string
  :group 'tfs)

(defcustom tfs-log-buffer-name "*TFS Log*"
  "Name of buffer for TFS Messages."
  :type 'string
  :group 'tfs)

(defvar tfs--status-last-used-dir "")
(defvar tfs--history-filename "")
(defvar tfs--process-name "TEE CLI")
(defvar tfs--changeset-buffer-name "*TFS Changeset*")
(defvar tfs--history-buffer-name "*TFS History*")

(defun tfs--get-or-create-process ()
  "Create or return the TEE process."
  (let ((buffer-name (concat "*" tfs--process-name "*")))
    (when (not (get-process tfs--process-name))
      (tfs--append-to-log "Creating new process instance...")
      (start-process tfs--process-name buffer-name
                     tfs-cmd "@"))
    (tfs--append-to-log "Returned process handle.")
    (get-process tfs--process-name)))

(defun tfs--process-command (command handler)
  "Set HANDLER as the filter function for the TEE CLI process and send COMMAND as string."
  (let* ((collection-param (tfs--get-collection-parameter))
         (login-param (tfs--get-login-parameter))
         (command-string (concat (mapconcat 'identity command " ") collection-param login-param "\n")))
    (set-process-filter (tfs--get-or-create-process) handler)
    (tfs--append-to-log "Command input: ")
    (tfs--append-to-log command-string)
    (process-send-string (tfs--get-or-create-process) command-string)))

(defun tfs--get-collection-parameter ()
  "Return the collection parameter if configured, or nil."
  (when (not (equal tfs-collection-url ""))
    (concat " -collection:" tfs-collection-url " ")))

(defun tfs--get-login-parameter ()
  "Return the login parameter if configured, or nil."
  (when (not (equal tfs-login ""))
    (concat " -login:" tfs-login " ")))

(defun tfs--message-callback (process output)
  "Show OUTPUT of PROCESS as message.  Also append to the TFS log."
  (tfs--append-to-log output)
  (message (concat "TFS:\n"  output)))

(defun tfs--determine-target-files (filename prompt)
  "Determine the name of the file to use in a TF command, or prompt for one.
If FILENAME, use it directly.  If called from a dired buffer it tries to
use the selection or the current file.  Else use PROMPT to get the user to
pick a file."
  (cond
   ((stringp filename)
    (list filename))
   ((eq major-mode 'dired-mode)
    (dired-get-marked-files))
   (buffer-file-name
    (list buffer-file-name))
   (t
    (list (expand-file-name (read-file-name prompt nil nil t))))))

(defun tfs--determine-target-directory (dirname  prompt)
  "Determine the name of a directory to use in a TF command, or prompt for one.
If DIRNAME, use it directly.  If called from a dired buffer it tries to
use the current directory.  Else use PROMPT to get the user to pick a dir."
  (cond
   ((stringp dirname)
    dirname)
   ((or (eq major-mode 'dired-mode) (buffer-file-name))
    default-directory)
   (t
    (expand-file-name (read-directory-name prompt nil nil t)))))

;; -------------------------------------------------------
;; tfs-checkout
;; performs a TFS checkout on a file.
(defun tfs-checkout (&optional filename)
  "Perform a tf checkout (edit).

The file to checkout is deteremined this way:

 - if FILENAME is specified, then this function selects that file
   to checkout.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file to checkout.

Checkout will occur only if the file is non-writable before the
call; checkout will fail if the specified file is currently
writable."
  (interactive)
  (let* ((files-to-checkout (tfs--determine-target-files filename "File to checkout: "))
         (command (append '("checkout") files-to-checkout)))
    (when files-to-checkout
      (message "Checking out file(s)...")
      (tfs--process-command command 'tfs--message-callback))))

(defun tfs-checkin ()
  "Perform a tf checkin on the file being visited by the current buffer.
Checkin happens only if the file is writable now.  This
function allows you to specify a checkin comment.  It checks in
only the current file being visited - pending changes for any
other files will not be checked in."
  (interactive)
  (if buffer-file-name
      (let* ((command (append '("checkin")
                              (tfs--checkin-parameters-builder)
                              (list (tfs--quote-string buffer-file-name)))))
        (tfs--process-command command 'tfs--message-callback))
    (error "Error tfs-checkin: No file")))

(defun tfs--checkin-parameters-builder ()
  "Build the parameters for the checkin command: comment, work item ID, overrie."
  (let* ((comment (read-string "Check in comment: "))
        (wid (read-string "Workitem ID (empty to skip): "))
        (override  (read-string "Override reason (empty to skip): "))
        (params (list (format "-comment:%s" (tfs--quote-string comment)))))
     (when (not (string-equal wid ""))
       (add-to-list 'params (format "-associate:%s" wid)))
     (when (not (string-equal override ""))
       (add-to-list 'params (format "-override:%s" (tfs--quote-string override))))
     params))

(defun tfs-rename (&optional filename new-name)
  "Perform a tf rename on a file.

The file to rename is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.  Only one file can be marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

The file is renamed to NEW-NAME, a string, which should be the
name of a non-existent file in the current directory, specified
without a qualifying path.

If the rename is successful, and if the buffer is visiting the
file that is being renamed, then this function also renames the
buffer to the new name."
  (interactive)
  (let ((file-to-rename (tfs--determine-target-files filename "File to rename: ")))
    (if (equal (length file-to-rename) 1)
        (let* ((source (car file-to-rename))
               (newname (or new-name (read-string (format "New name for %s: " source) nil nil nil)))
               (command (list "rename" source newname)))
          (tfs--process-command command 'tfs--message-callback)
          (if (string-equal source buffer-file-name)
              (set-visited-file-name newname)
            (error "Rename of %s was unsuccessful" file-to-rename)))
      (error "Couldn't determine file to rename"))))

(defun tfs-add (&optional filename)
  "Perform a tf add on a file.

The file to add is deteremined this way:

 - if FILENAME is specified, then this function selects that file
   to add.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file to add."
  (interactive)
  (let ((files-to-add (tfs--determine-target-files filename "File(s) to add: ")))
    (if files-to-add
        (let* ((items (mapcar 'tfs--quote-string files-to-add))
               (command (append '("add") items)))
          (tfs--process-command command 'tfs--message-callback))
      (error "Error tfs-add: No file"))))

(defun tfs-delete (&optional filename)
  "Perform a tf delete on a file.

The file to delete is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

If the delete is successful, and if the buffer is visiting the file that
is being deleted, then this function also kills the buffer."
  (interactive)
  (let ((files-to-delete (tfs--determine-target-files filename "File to delete: ")))
    (if files-to-delete
        (let* ((items (mapcar 'tfs--quote-string files-to-delete))
               (command (append '("delete") items)))
          (tfs--process-command command 'tfs--message-callback))
      (error "Error tfs-delete: No file"))))

(defun tfs-get (&optional filename)
  "Perform a tf get on a file.

The file to get is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file."
  (interactive)
  (let ((files-to-get (tfs--determine-target-files filename "File(s) to get: ")))
    (when files-to-get
        (let* ((items (mapcar 'tfs--quote-string files-to-get))
               (version (list (tfs--get-version-param)))
               (command (append '("get") files-to-get version)))
          (tfs--process-command command 'tfs--message-callback)))))

(defun tfs--get-version-param ()
  "Get a version spec string for a command that supports it."
  (let ((param " -version:")
        (value (read-string "Version spec (blank for latest): ")))
    (if (string-equal value "")
        (concat param "T ")
      (concat param value " "))))

(defun tfs-get-recursive (&optional dirname)
  "Perform a recursive tf get on a directory.

The directory to get is deteremined this way:

 - if DIRNAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the current directory.

 - when there is a file backing the current buffer, it selects
   the file's directory

 - else, prompt the user for a dir."
  (interactive)
  (let ((dir-to-get (tfs--determine-target-directory dirname "Directory to get: ")))
    (when dir-to-get
        (let* ((command (list "get" (tfs--quote-string dir-to-get) "-recursive")))
          (tfs--process-command command 'tfs--message-callback)))))


(defun tfs-undo (&optional filename)
  "Perform a tf undo on a file.

The file to undo is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file."
  (interactive)
  (let ((files-to-undo (tfs--determine-target-files filename "File(s) to undo: ")))
    (when files-to-undo
        (let* ((items (mapcar 'tfs--quote-string files-to-undo))
                (command (append '("undo") items)))
             (when (yes-or-no-p "Undo changes to file(s)? ")
               (tfs--process-command command 'tfs--message-callback))))))

(defun tfs--history-parameters-builder ()
  "Build the parameters for the history command: stopafter and user."
  (let ((user (read-string "Filter by user (blank to ignore): "))
        (stopafter (string-to-number (read-string "Number of items to retrieve (blank for 50): ")))
        (params nil))
    (when (equal stopafter 0)
      (setq stopafter 25))
    (setq params (list (concat " -stopafter:" (number-to-string stopafter) " ")))
    (when (not (string-equal user ""))
       (add-to-list 'params (format "-user:%s " user)))
     params))

(defun tfs-history (&optional filename)
  "Perform a tf history on a file.
How the file is determined:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.  Only one file can be marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file"
  
  (interactive)
  (let ((files-for-history (tfs--determine-target-files filename "File: ")))
    (if (equal (length files-for-history) 1)
        (let* ((source (car files-for-history))
               (command (list "history" source)))
          (setq command (append command (tfs--history-parameters-builder)))
          (when (get-buffer tfs--history-buffer-name)
            (kill-buffer tfs--history-buffer-name))
          (message "TFS: Getting item history...")
          (setq tfs--history-filename source)
          (tfs--process-command command 'tfs--history-callback))
      (error "Couldn't determine history target"))))

(defun tfs--history-callback (process output)
  "Show the buffer with the history command result.
PROCESS is the TEE process
OUTPUT is the raw output"
  (let* ((buffer (get-buffer-create tfs--history-buffer-name)))
    (with-current-buffer buffer
      (when (equal (buffer-size) 0)
        (insert (concat "History for item " tfs--history-filename "\n\n")))
      (insert output)
      (display-buffer tfs--history-buffer-name t))))

(defun tfs-changeset ()
  "Gets info on a changeset."
  (interactive)
  (let ((version (read-string "Changeset number: (blank for latest): ")))
    (when (string-equal version "")
      (setq version "-latest"))
    (when (get-buffer tfs--changeset-buffer-name)
      (kill-buffer tfs--changeset-buffer-name))
    (message "TFS: Getting changeset details...")
    (tfs--process-command (list "changeset" version) 'tfs--changeset-callback)))

(defun tfs--changeset-callback (process output)
  "Show the buffer with the changeset command result.
PROCESS is the TEE process
OUTPUT is the raw output"
  (let* ((buffer (get-buffer-create tfs--changeset-buffer-name)))
    (with-current-buffer buffer
      (insert output)
      (display-buffer tfs--changeset-buffer-name t))))

(define-derived-mode tfs-status-mode tabulated-list-mode "TFS Status Mode" "Major mode TFS Status, displays current pending changes"
  (setq tabulated-list-format [("Change" 7 t)
                               ("Local Path" 100 t)
                               ("Server Path" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Local Path" nil))
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun tfs--quote-string (param)
  "Surround PARAM with quotes using format.  Useful for paths and comments."
  (format "\"%s\"" param))

(defun tfs--status-mode-checkin ()
  "Process files marked in tfs-status-mode for check in."
  (interactive)
  (let* ((items (tfs--status-mode-marked-items))
         (command (append '("checkin") (tfs--checkin-parameters-builder) items)))
    (tfs--process-command command 'tfs--message-callback)))

(defun tfs--status-mode-revert ()
  "Revert (undo) the files marked using tfs-status-mode."
  (interactive)
  (let* ((items (tfs--status-mode-marked-items))
         (quoted-items (mapcar 'tfs--quote-string items))
         (command '("undo")))
    (when (yes-or-no-p "Undo changes to the  files marked? ")
      (setq command (append command quoted-items))
      (tfs--process-command command 'tfs--message-callback))))

(defun tfs--status-mode-marked-items ()
  "Obtain only the path of the files selected in the list."
  (mapcar 'tfs--quote-string (mapcar 'car (tablist-get-marked-items))))

(defun tfs--status-mode-visit-item ()
  "Visit the file under the cursor in tfs-status-mode."
  (interactive)
  (find-file (tabulated-list-get-id)))

(define-key tfs-status-mode-map (kbd "C") 'tfs--status-mode-checkin)
(define-key tfs-status-mode-map  (kbd "R") 'tfs--status-mode-revert)
(define-key tfs-status-mode-map (kbd "g") 'tfs--status-mode-reload-last-dir)
(define-key tfs-status-mode-map (kbd "RET") 'tfs--status-mode-visit-item)

(defun tfs-pending-changes ()
  "Perform a recursive tf status.  Displays the result in a separate buffer."
  (interactive)
  (let* ((status-dir (tfs--select-status-directory)))
    ; To refresh the status when using "g" in the changes buffer
    (setq tfs--status-last-used-dir status-dir)
    (tfs--status-mode-reload-last-dir)))

(defun tfs--pendingchanges (directory)
  "Internal call to run the status command  in DIRECTORY."
  (let* ((command (list "status" directory "-recursive" "-nodetect"  "-format:xml")))
    (message "Obtaining list of pending changes...")
    (tfs--process-command command 'tfs--status-callback)))

(defun tfs--status-mode-reload-last-dir ()
  "Reload tfs-status-mode with the last successful directry invoked."
  (interactive)
  (tfs--pendingchanges tfs--status-last-used-dir))

(defun tfs--status-callback (process output)
  "Process the output of tf status and display the tfs-status-mode buffer.
PROCESS is the TEE process
OUTPUT is the raw output"
  (let* ((status-bufname  "*TFS Pending Changes*")
         (buffer (get-buffer-create status-bufname)))
    (with-current-buffer buffer
      (setq inhibit-read-only t)
      (erase-buffer)
      (setq tabulated-list-entries (tfs--get-status-data-for-tablist output))
      (tfs-status-mode)
      (tablist-revert)
      (switch-to-buffer buffer))))

(defun tfs--get-status-data-for-tablist (xml-status-data)
  "Format XML-STATUS-DATA from the status command for tabulated list."
  (with-temp-buffer
    (insert xml-status-data)
    (let* ((converted-xml-data (libxml-parse-xml-region (point-min) (point-max)))
           (pending-change-nodes (dom-by-tag converted-xml-data 'pending-change)))
      (mapcar 'tfs--format-status-node pending-change-nodes))))

(defun tfs--format-status-node (pending-changes-node)
  "Extract from PENDING-CHANGES-NODE the info."
  (let ((data (cadr pending-changes-node)))
    (list
     (alist-get 'local-item data)
     (vector
      (alist-get 'change-type data)
      (alist-get 'local-item data)
      (alist-get 'server-item data)))))

(defun tfs--select-status-directory ()
  "Prompt for a directory.  Try  projectile root first, else use current buffer's directory."
  (let ((tfs-status-dir "")
        (default-dir-prompt "?"))
    (ignore-errors
      (when (fboundp 'projectile-project-root)
        (setq default-dir-prompt (projectile-project-root))))
    (when (string= default-dir-prompt "?")
      (setq default-dir-prompt default-directory))
    (read-directory-name "Status for directory: " default-dir-prompt nil t)))

(defun tfs--append-to-log (text)
  "Append TEXT to the TFS Messages buffer.
Intended for internal use only."
  (let ((buf (current-buffer))
        (tfsbuffer (get-buffer-create tfs-log-buffer-name)))
    (set-buffer tfsbuffer)
    (goto-char (point-max))
    (insert text)
    (insert "\n")
    (set-buffer buf)))


(provide 'tfs)

;;; tfs.el ends here
