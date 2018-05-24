;;; tfsmacs.el --- MS Team Foundation Server commands.
;; URL        : https://github.com/sebasmonia/tfs.el
;; Authors    : Dino Chiesa <dpchiesa@outlook.com>, Sebastian Monia <smonia@outlook.com>
;; Version    : 20180524
;; Package-Requires: ((emacs "25") (tablist "0.70"))
;;; Commentary:
;;
;; Basic steps to setup:
;;   1. Obtain the Team Explorer Everywhere CLI tool from https://github.com/Microsoft/team-explorer-everywhere/releases
;;   2. Place `tfsmacs.el' in your `load-path'.
;;   3. In your .emacs file:
;;        (require 'tfsmacs)
;;        (setq tfs-cmd  "location/of/TEE/tf")
;;        (setq tfs-login "/login:domain\\userid,password")
;;   4. Also in your .emacs file,  set local or global key bindings for tfs commands.
;;      Example:
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
  "Name of the TFS log buffer."
  :type 'string
  :group 'tfs)

(defvar tfs--status-last-used-dir "")
(defvar tfs--history-target "")
(defvar tfs--file-output-target "TEE CLI")
(defvar tfs--process-name "TEE CLI")
(defvar tfs--changeset-buffer-name "*TFS Changeset*")
;; These two variables are used to accumulate the output of their respective commands.
;; Once the output can be parsed, that means the XML is completed and the command finished
;; processing. Then the variables are emptied for the next call.
;; I couldn't find a better way since "tf @" doesn't show a prompt. I could try something like
;; comint-mode but this seemed simpler than changing the way the underlying process works.
(defvar tfs--history-xml-buffer "")
(defvar tfs--status-xml-buffer "")

(defun tfs--get-or-create-process ()
  "Create or return the TEE process."
  (let ((buffer-name (concat "*" tfs--process-name "*")))
    (when (not (get-process tfs--process-name))
      (tfs--append-to-log "Creating new process instance...")
      (start-process tfs--process-name buffer-name
                     tfs-cmd "@"))
    (tfs--append-to-log "Returned process handle.")
    (get-process tfs--process-name)))

(defun tfs--process-command-sync (command)
  "Create a new instance of the TEE process to execute COMMAND and block until output is done."
  (let* ((collection-param (tfs--get-collection-parameter))
         (login-param (tfs--get-login-parameter))
         (params (append command (list collection-param login-param))))
    (tfs--append-to-log (concat "Command input (sync): " (prin1-to-string params)))
    (apply 'process-lines tfs-cmd params)))

(defun tfs--process-command (command handler)
  "Set HANDLER as the filter function for the TEE CLI process and send COMMAND as string."
  (let* ((collection-param (tfs--get-collection-parameter))
         (login-param (tfs--get-login-parameter))
         (command-string (concat (mapconcat 'identity command " ") collection-param login-param "\n")))
    (set-process-filter (tfs--get-or-create-process) handler)
    (tfs--append-to-log (concat "Command input: " command-string))
    (process-send-string (tfs--get-or-create-process) command-string)))

(defun tfs--get-collection-parameter ()
  "Return the collection parameter if configured, or empty string."
  (if (not (string-equal tfs-collection-url ""))
      (concat " -collection:" tfs-collection-url " ")
    ""))

(defun tfs--get-login-parameter ()
  "Return the login parameter if configured, or empty string."
  (if (not (string-equal tfs-login ""))
      (concat " -login:" tfs-login " ")
    ""))

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

(defun tfs--last-dir-name (path)
  "Return only the last directory name in PATH.
From: https://stackoverflow.com/questions/27284851/emacs-lisp-get-directory-name-not-path-from-the-path"
  (file-name-nondirectory
   (directory-file-name
     (file-name-directory path))))

(defun tfs--write-file-to-temp-directory (path version)
  "Write the VERSION of PATH to a temporary directory.
It spins off a new instance of the TEE tool by calling 'tfs--process-command-sync'"
  ;remove quotes around  path if needed.
  (when (string-equal "\"" (substring path 0 1))
    (setq path (substring path 1 -1)))
  (let* ((only-name (file-name-nondirectory path))
         (filename (concat temporary-file-directory version ";" only-name))
         (command (list "print" (concat "-version:" version) path))
         (content nil))
    (setq content (tfs--process-command-sync command))
    (with-temp-file filename
      (insert (mapconcat 'identity content "\n")))
    filename))

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
      (message "TFS: Checking out file(s)...")
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

(defun tfs-get (&optional filename version)
  "Perform a tf get on a file.

The file to get is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the files marked, or the file on the
   current line in none marked.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

If VERSION to get is not provided, it will be prompted."
  (interactive)
  (let ((files-to-get (tfs--determine-target-files filename "File(s) to get: ")))
    (when files-to-get
        (let* ((items (mapcar 'tfs--quote-string files-to-get))
               (version (list (tfs--get-version-param version)))
               (command (append '("get") files-to-get version)))
          (tfs--process-command command 'tfs--message-callback)))))

(defun tfs--get-version-param (&optional version)
  "Get a version spec string for a command that supports it.
If VERSION is provided return said version as changeset."
  (let ((param " -version:"))
    (when (not version)
      (setq version (read-string "Version spec (blank for latest): ")))
    (if (string-equal version "")
        (concat param "T ")
      (concat param version " "))))

(defun tfs-get-recursive (&optional dirname force)
  "Perform a recursive tf get on a directory.
Use FORCE (or prefix arg) to overwrite writeable files not checked out
and get even up-to-date files.

The directory to get is deteremined this way:

 - if DIRNAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the current directory.

 - when there is a file backing the current buffer, it selects
   the file's directory

 - else, prompt the user for a dir."
  (interactive)
  (when current-prefix-arg
    (setq force t))
  (let* ((dir-to-get (tfs--determine-target-directory dirname "Directory to get: "))
         (command (list "get" "-recursive" (tfs--quote-string dir-to-get))))
    (when force
      (setq command (append command '("-force"))))
    (when dir-to-get
      (tfs--process-command command 'tfs--message-callback))))


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

(define-derived-mode tfs-history-mode tabulated-list-mode "TFS history Mode" "Major mode TFS History, displays an item's history and allows compares."
  (setq tabulated-list-format [("Changeset" 10 t)
                               ("Type" 8. t)
                               ("Date" 20 t)
                               ("Committed by" 30 t)
                               ("Comment" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Changeset" t))
  (tabulated-list-init-header)
  (tablist-minor-mode))

(define-key tfs-history-mode-map (kbd "T") 'tfs--history-mode-get-this-version)
(define-key tfs-history-mode-map (kbd "C") 'tfs--history-mode-changeset-details)
(define-key tfs-history-mode-map (kbd "D") 'tfs--history-mode-difference)


(defun tfs--history-mode-quote-path (item-pair)
  "Return the same ITEM-PAIR with only the path quoted."
  (list
   (car item-pair)
   (tfs--quote-string (cadr item-pair))))

(defun tfs--history-mode-get-marked-items ()
  "Return the selected items in ‘tfs-history-mode’."
  (mapcar 'tfs--history-mode-quote-path (mapcar 'car (tablist-get-marked-items))))

(defun tfs--history-mode-get-this-version ()
  "Get the file version marked/selected in the ‘tfs-history-mode’ buffer."
  (interactive)
  (let* ((items (tfs--history-mode-get-marked-items))
         (to-get (car items)))
    (if (equal (length items) 1)
        (progn
          (message (concat "TFS: Getting changeset " (car to-get)))
          (tfs-get (cadr to-get) (car to-get)))
      (error "Only one item should be selected for this operation"))))

(defun tfs--history-mode-changeset-details ()
  "Open changeset details for the selected item in ‘tfs-history-mode’."
  (interactive)
  (let* ((items (tfs--history-mode-get-marked-items))
         (to-get (car items)))
    (if (equal (length items) 1)
        (progn
          (message (concat "TFS: Getting changeset details " (car to-get)))
          (tfs-changeset (car to-get)))
      (error "Only one item should be selected for this operation"))))

(defun tfs--history-mode-difference ()
  "Compare two selected items in ‘tfs-history-mode’."
  (interactive)
  (let* ((items (tfs--history-mode-get-marked-items))
         (first-item (car items))
         (second-item (cadr items)))
    (if (equal (length items) 2)
        (progn
          (tfs--history-mode-ediff first-item second-item))
      (error "Only two items should be selected for this operation"))))

(defun tfs--history-mode-ediff (file1 file2)
  "Compares FILE1 and FILE2 using ediff."
  (message "TFS: Retrieving files to compare. This operation can take a few seconds.")
  (let* ((temp1 (tfs--write-file-to-temp-directory (cadr file1) (car file1)))
         (temp2 (tfs--write-file-to-temp-directory (cadr file2) (car file2))))
    (ediff-files temp1 temp2)))

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
          (message "TFS: Getting item history...")
          (setq tfs--history-target source)
          (setq tfs--history-xml-buffer "") ; just in case a previous invocation went wrong :)
          (tfs--process-command command 'tfs--history-callback))
      (error "Couldn't determine history target"))))

(defun tfs--history-parameters-builder ()
  "Build the parameters for the history command: stopafter and user."
  (let ((user (read-string "Filter by user (blank to ignore): "))
        (stopafter (string-to-number (read-string "Number of items to retrieve (blank for 50): ")))
        (params (list "-recursive" "-format:xml")))
    (when (equal stopafter 0)
      (setq stopafter 50))
    (add-to-list 'params (concat " -stopafter:" (number-to-string stopafter) " "))
    (when (not (string-equal user ""))
       (add-to-list 'params (format "-user:%s " user)))
     params))

(defun tfs--history-callback (process output)
  "Process the output of tf history and display the ‘tfs-history-mode’ buffer.
PROCESS is the TEE process
OUTPUT is the raw output"
  (setq tfs--history-xml-buffer (concat tfs--history-xml-buffer output))
  (let ((parsed-data (tfs--get-history-data-for-tablist tfs--history-xml-buffer)))
    (when parsed-data
      (setq tfs--history-xml-buffer "")
      (let* ((short-target (file-name-nondirectory tfs--history-target))
             (history-bufname (concat "*TFS History " short-target "*"))
             (buffer (get-buffer-create history-bufname)))
             (with-current-buffer buffer
               (setq tabulated-list-entries parsed-data)
               (tfs-history-mode)
               (tablist-revert)
               (switch-to-buffer buffer))))))

(defun tfs--get-history-data-for-tablist (xml-status-data)
  "Format XML-STATUS-DATA from the history command for tabulated list."
  (with-temp-buffer
    (insert xml-status-data)
    (let* ((converted-xml-data (libxml-parse-xml-region (point-min) (point-max)))
           (changeset-nodes (dom-by-tag converted-xml-data 'changeset)))
      (mapcar 'tfs--format-history-node changeset-nodes))))

(defun tfs--format-history-node (changeset-node)
  "Extract from CHANGESET-NODE the info."
  (let ((changeset (cadr changeset-node))
        (comment (car (dom-by-tag changeset-node 'comment)))
        (item (cadr (car (dom-by-tag changeset-node 'item)))))
    (setq comment (nth 2 comment))
    (when (not comment)
      (setq comment ""))
    (list
     (list (alist-get 'id changeset) (alist-get 'server-item item))
     (vector
      (alist-get 'id changeset)
      (alist-get 'change-type item)
      (tfs--format-history-node-date (alist-get 'date changeset))
      (alist-get 'committer changeset)
      comment))))

(defun tfs--format-history-node-date (date-string)
  "Convert DATE-STRING to a better representation."
  ; Maybe it is worth it to do proper date formatting. TODO?
  (replace-regexp-in-string "T" " " (substring date-string  0 -9)))

(defun tfs-changeset (&optional version)
  "Gets info on a changeset.
If VERSION to get is not provided, it will be prompted."
  (interactive)
  (when (not version)
    (setq version (read-string "Changeset number: (blank for latest): ")))
  (when (string-equal version "")
      (setq version "-latest"))
  (when (get-buffer tfs--changeset-buffer-name)
    (kill-buffer tfs--changeset-buffer-name))
  (message "TFS: Getting changeset details...")
  (tfs--process-command (list "changeset" version) 'tfs--changeset-callback))

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
  "Process files marked in ‘tfs-status-mode’ for check in."
  (interactive)
  (let* ((items (tfs--status-mode-get-marked-items))
         (command (append '("checkin") (tfs--checkin-parameters-builder) items)))
    (tfs--process-command command 'tfs--message-callback)))

(defun tfs--status-mode-revert ()
  "Revert (undo) the files marked using ‘tfs-status-mode’."
  (interactive)
  (let* ((items (tfs--status-mode-get-marked-items))
         (quoted-items (mapcar 'tfs--quote-string items))
         (command '("undo")))
    (when (yes-or-no-p "Undo changes to the  files marked? ")
      (setq command (append command quoted-items))
      (tfs--process-command command 'tfs--message-callback))))

(defun tfs--status-mode-difference ()
  "Compares pending change to latest version."
  (interactive)
  (let ((items (tfs--status-mode-get-marked-items)))
    (if (equal (length items) 1)
        (progn
          (message "TFS: Retrieving files to compare. This operation can take a few seconds.")
          (let* ((local (substring (car items) 1 -1)) ; these items are always quoted, remove the quotes
                 (server (tfs--write-file-to-temp-directory local "T")))
            (ediff-files local server)))
      (error "Select only one file to compare to latest version"))))

(defun tfs--status-mode-get-marked-items ()
  "Obtain only the path of the files selected in the list."
  (mapcar 'tfs--quote-string (mapcar 'car (tablist-get-marked-items))))

(defun tfs--status-mode-visit-item ()
  "Visit the file under the cursor in ‘tfs-status-mode’."
  (interactive)
  (find-file (tabulated-list-get-id)))

(define-key tfs-status-mode-map (kbd "C") 'tfs--status-mode-checkin)
(define-key tfs-status-mode-map  (kbd "R") 'tfs--status-mode-revert)
(define-key tfs-status-mode-map (kbd "g") 'tfs--status-mode-reload-last-dir)
(define-key tfs-status-mode-map (kbd "RET") 'tfs--status-mode-visit-item)
(define-key tfs-status-mode-map  (kbd "D") 'tfs--status-mode-difference)
;(define-key tfs-status-mode-map (kbd "RET") 'tfs--status-mode-shelve)

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
    (message "TFS: Obtaining list of pending changes...")
    (setq tfs--status-xml-buffer "") ; just in case a previous invocation went wrong :)
    (tfs--process-command command 'tfs--status-callback)))

(defun tfs--status-mode-reload-last-dir ()
  "Reload ‘tfs-status-mode’ with the last successful directry invoked."
  (interactive)
  (tfs--pendingchanges tfs--status-last-used-dir))

(defun tfs--status-callback (process output)
  "Process the output of tf status and display the ‘tfs-status-mode’ buffer.
PROCESS is the TEE process
OUTPUT is the raw output"
  (setq tfs--status-xml-buffer (concat tfs--status-xml-buffer output))
  (let ((parsed-data (tfs--get-status-data-for-tablist tfs--status-xml-buffer)))
    (when parsed-data
      (setq tfs--status-xml-buffer "")
      (let* ((dir-name (tfs--last-dir-name tfs--status-last-used-dir))
             (status-bufname (concat "*TFS Status " dir-name "*"))
             (buffer (get-buffer-create status-bufname)))
        (with-current-buffer buffer
          (setq tabulated-list-entries parsed-data)
          (tfs-status-mode)
          (tablist-revert)
          (switch-to-buffer buffer))))))

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


(provide 'tfsmacs)

;;; tfsmacs.el ends here
