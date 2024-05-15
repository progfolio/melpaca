;;; melpaca.el --- CI tool for MELPA  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicholas Vollmer

;; Author: Nicholas Vollmer) <nv@parenthetic.dev>
;; Keywords: convenience, tools
;; Homepage: https://www.github.com/progfolio/melpaca
;; Package-Requires: ((emacs "27.1") (elpaca "0.0.2"))
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Proof of concept CI review tool for MELPA PRs

;;

;;; Code:
(require 'elpaca)
(require 'elpaca-test)
(require 'elpaca-menu-melpa)

(defgroup melpaca nil "An Elpaca-powered MELPA Feedback CI tool."
  :prefix "melpaca-"
  :group 'applications)

(defcustom melpaca-after-test-function #'melpaca--display-results
  "Function run once test is finished."
  :type 'function)

(defcustom melpaca-repo "melpa/melpa" "USER/REPO to request PR info from." :type 'string)

(defvar melpaca-generic-fetchers '(git hg))
(defvar melpaca-fetchers (append melpaca-generic-fetchers '(github gitlab codeberg sourcehut)))
(defvar melpaca-poll-interval 0.1)
(defvar melpaca-test-timeout 60)
(defvar melpaca--blocking nil)

(defun melpaca--validate-id (object)
  "Signal error if OBJECT is not a valid MELPA recipe ID."
  (let ((warnings nil)
        (id (or (car-safe object) (error "Reicpe must be a non-nil list"))))
    (unless (symbolp id) (error "Recipe ID %S must be a symbol" id))
    (unless (> (length (symbol-name id)) 2)
      (push (cons 'warning (format "Recipe ID %s should be longer than 2 characters" id))
            warnings))
    ;; @TODO: Make configurable depending on type of PR?
    ;; We could error unless PR description is an update to existing recipe.
    (when (elpaca-menu-item id) ;;@TODO: hg type support in menu
      (push (cons 'warning (format "Recipe ID %s already present in MELPA archive" id))
            warnings))
    (when (alist-get id package--builtin-versions)
      (push (cons 'warning (format "Recipe ID %s shadows built-in Emacs package" id))
            warnings))
    (when-let ((repo (plist-get :repo (cdr object)))
               (name (cadr (split-string repo "/" )))
               ((not (equal (file-name-sans-extension name) (symbol-name id)))))
      (push (cons 'warning (format "Recipe ID %s does not match :repo name %s" id name))
            warnings))
    warnings))

(defun melpaca--validate-files (files)
  "Signal error if FILES is not a valid MELPA recipe :files list."
  (cl-loop
   with excludedp with i = -1
   for el in (or (and (consp files) files)
                 (error ":files should be non-nil list"))
   for position = (cl-incf i)
   unless
   (or (stringp el)
       (and (eq el :defaults)
            (or (zerop position) (error ":files :defaults must be first element")))
       (and (eq (car-safe el) :exclude)
            (if excludedp (error ":files must include only one :exclude element")
              (setq excludedp t)))
       (and (consp el)
            (or (cl-every #'stringp el)
                (error ":files malformed element %S" el))))
   do (error ":files must be a list of \
 (STRING|:defaults [STRING|(STRING...)...] [(:exclude STRING...)])")))

(defun melpaca-validate-recipe (object)
  "Validate recipe OBJECT."
  (condition-case err
      (cl-destructuring-bind ( &key fetcher url repo commit branch version-regexp files old-names
                               &aux (warnings (melpaca--validate-id object)))
          (or (cdr object) (error "No :fetcher"))
        (unless (memq fetcher melpaca-fetchers)
          (error ":fetcher must be one of the following symbols: %S" melpaca-fetchers))
        (if (memq fetcher melpaca-generic-fetchers)
            (progn (when repo (error ":repo incompatible with :fetcher %s" fetcher))
                   (or (stringp url) (error ":fetcher %s requires :url string" fetcher)))
          (when url (error ":url incompatible with :fetcher %s" fetcher)))
        ;;@TODO check maintainer against github user name?
        (unless (or (memq fetcher melpaca-generic-fetchers)
                    (and (stringp repo) (string-match-p "[^/]+/[^/]+" repo)))
          (error ":repo must be a string of form \"user-name/repo-name\""))
        (when (and (eq fetcher 'sourcehut) (string-match-p "~" repo))
          (error ":repo must not include \"~\" in Sourcehut user-name"))
        (when (and branch commit) (error "Cannot use :branch and :commit at same time"))
        (when commit
          (when (eq fetcher 'hg) (push (cons 'warning (format ":fetcher hg ignores :commit %s" commit))
                                       warnings))
          (unless (stringp commit) (error ":commit must be a string")))
        (when branch
          ;;@TODO: :branch valid for :fetcher hg?
          (unless (stringp branch) (error ":branch must be a string")))
        (when version-regexp
          (push (cons 'warning ":version-regexp rarely necessary") warnings)
          (unless (stringp version-regexp) (error ":verson-regexp must be a string")))
        (when files
          (push (cons 'warning ":files rarely necessary") warnings)
          (melpaca--validate-files files))
        (when old-names (unless (and (listp old-names) (cl-every #'symbolp old-names))
                          (error ":old-names must be a list of symbols")))
        warnings)
    ((error) (list (cons 'error (cadr err))))))

(declare-function elpaca-log "elpaca-log")
(defun melpaca--test (heading type results)
  "Print HEADING, test RESULTS of TYPE.
Return t if test passes, nil otherwise."
  (let* ((statuses (mapcar #'car results))
         (status (cond ((memq 'error statuses) 'error)
                       ((memq 'warning statuses) 'warning)))
         (status-symbols '((error . "❌") (warning . "⚠️"))))
    (princ (concat "\n"
                   (when status "<details open><summary>")
                   (alist-get status status-symbols "✅") " " heading
                   (when status "</summary>")
                   "\n"))
    (when status
      (princ (format "\n```%s\n%s\n```\n</details>"
                     (or type "")
                     (mapconcat (lambda (r)
                                  (format "%s"
                                          ;;(alist-get (car r) status-symbols)
                                          (cdr r)))
                                results "\n"))))
    (not (eq status 'error))))

(defvar package-archives)
(defun melpaca--init-package-lint ()
  "Workaround for package-lint's dependency on `package-archives-contents'."
  (interactive)
  (elpaca (package-lint :wait t))
  (require 'package-lint)
  (setq package-user-dir (make-temp-file "elpa" 'directory))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (let ((inhibit-message t))
    (package-initialize)
    (package-refresh-contents)))

(defvar url-http-response-status)
(defun melpaca--validate-upstream-url (url)
  "If URL is unreachable return test error info."
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously url 'silent t 30)
        (unless (equal url-http-response-status 200)
          (cons 'error (format "%S returned statsus %S" url url-http-response-status))))
    ((error) (list (cons 'error err)))))

(defun melpaca--display-results ()
  "Print test results."
  (pop-to-buffer (current-buffer) '((display-buffer-reuse-window))))

(defvar melpaca--location (or load-file-name (buffer-file-name)))
(declare-function package-lint-buffer "package-lint")
(defun melpaca--run-tests (recipe)
  "Test RECIPE."
  (let* ((id (car recipe))
         (pkg (symbol-name id))
         (regexp (concat "^" pkg)))
    (and
     (melpaca--test
      "Package installs" 'emacs-lisp
      (condition-case err
          (progn (elpaca-try recipe)
                 (elpaca-wait)
                 (when-let ((e (elpaca-get id))
                            ((eq (elpaca--status e) 'failed)))
                   (list (cons 'error (nth 2 (car (elpaca<-log e)))))))
        ((error) (list (cons 'error err)))))
     (melpaca--test
      "Package compiles cleanly" 'emacs-lisp
      (with-current-buffer (elpaca-log (concat regexp " | byte-comp | Warning\\|Error") t)
        (cl-loop for entry in tabulated-list-entries
                 for info = (aref (cadr entry) 2)
                 collect (cons (if (string-match-p "Warning" info) 'warning 'error)
                               info))))
     (let* ((e (elpaca-get id))
            (main (elpaca<-main e)))
       (find-file (expand-file-name main (elpaca<-repo-dir e)))
       (melpaca--test
        (format "%s satisfies package-lint" main) 'emacs-lisp
        (cl-loop for (line col type message) in (package-lint-buffer)
                 collect (cons type (format "%s:%s %s" line col message))))))))

(defvar url-http-end-of-headers)
(defun melpaca-pull-request (number)
  "Return data for pull request with NUMBER."
  (unless melpaca-repo (error "Nil melpaca-repo"))
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Accept" . "application/vnd.github+json")))
        (request (format "https://api.github.com/repos/%s/pulls/%s" melpaca-repo number)))
    (message "requesting: %S" request)
    (with-current-buffer (url-retrieve-synchronously request)
      (goto-char url-http-end-of-headers)
      (json-parse-buffer :object-type 'alist :null-object nil :false-object nil))))

(defun melpaca--diff-to-recipe (url)
  "Return recipe from diff URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (unless (equal url-http-response-status 200)
      (error "Unable to download %S %S" url url-http-response-status))
    (goto-char (point-max))
    (backward-list)
    (read (current-buffer))))

(cl-defun melpaca (number)
  "Provide feedback for Pull Request with NUMBER."
  (with-current-buffer (get-buffer-create "*melpaca*")
    (let* ((pr (melpaca-pull-request number))
           (_ (when-let ((message (alist-get 'message pr)))
                (error "Github API Response: %s" message)))
           (recipe (melpaca--diff-to-recipe (alist-get 'diff_url pr)))
           (standard-output (current-buffer)))
      (princ (format "Testing Recipe\n\n```emacs-lisp\n%S\n```\n" recipe))
      (and
       (melpaca--test
        "Submission contains 1 recipe" nil
        (unless (= (alist-get 'changed_files pr) 1)
          (list (cons 'error "Submission contains more than 1 recipe.
Please submit 1 recipe at a time."))))
       (melpaca--test "Package recipe valid" nil (melpaca-validate-recipe recipe))
       ;;(melpaca--test "Package upstream reachable" nil (melpaca--validate-upstream-url link))
       (let ((melpaca--blocking t)
             (elpaca-test-start-functions nil)
             (elpaca-test-finish-functions
              (lambda (&rest _) (setq melpaca--blocking nil))))
         (eval `(elpaca-test
                  :ref local
                  :buffer "*melpaca*"
                  :timeout ,melpaca-test-timeout
                  :early-init (setq elpaca-menu-functions '(elpaca-menu-melpa))
                  :init
                  (load ,melpaca--location nil 'nomessage) ;;Load this version of melpaca
                  (melpaca--init-package-lint)
                  (melpaca--run-tests ',recipe))
               t)
         (while melpaca--blocking (sit-for melpaca-poll-interval))))
      (funcall melpaca-after-test-function))))

;;@TODO: Report git-blamed :old-names incorrect element type string
;;@TODO: License Check
;;@TODO: Extra feedback?
;;@TODO: Previous art suggestions

(provide 'melpaca)
;;; melpaca.el ends here
