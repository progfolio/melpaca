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
(require 'elpaca-log)
(require 'elpaca-test)
(require 'elpaca-menu-melpa)

(defgroup melpaca nil "An Elpaca-powered MELPA Feedback CI tool."
  :prefix "melpaca-"
  :group 'applications)

(defcustom melpaca-after-test-function #'melpaca--display-results
  "Function run once test is finished."
  :type 'function)

(defcustom melpaca-repo "melpa/melpa" "USER/REPO to request PR info from." :type 'string)

(defcustom melpaca-markdown-section-regexp "\\(?:^[[:space:]]*###[^z-a]*?$\\)"
  "Regexp for splitting PR markdown post into sections."
  :type 'string)

(defcustom melpaca-pr-post-sections '(description url associations)
  "List of PR post sections."
  :type '(list symbol))

(defcustom melpaca-test-status-indicators '((error . "❌") (warning . "⚠️") (pass . "✅"))
  "Alist of form ((STATUS . STRING)...)."
  :type 'alist)

(cl-defstruct (melpaca-test (:constructor melpaca-test) (:copier nil) (:named))
  "Test struct."
  title required syntax output warnings errors)

(defun melpaca-print-test (test)
  "Print TEST."
  (let* ((warnings (melpaca-test-errors test))
         (errors (melpaca-test-errors test))
         (output (melpaca-test-output test))
         (summary (format "%s %s\n\n"
                          (alist-get (cond (errors 'error) (warnings 'warning) (t 'pass))
                                     melpaca-test-status-indicators)
                          (melpaca-test-title test)))
         (results (append warnings output)))
    (princ "\n")
    (if (not results) (princ summary)
      (princ (format "<details%s><summary>%s</summary>\n\n```%s\n%s\n```\n</details>\n"
                     (if (or warnings errors) " open" "")
                     (string-trim summary)
                     (or (melpaca-test-syntax test) "")
                     (mapconcat (lambda (el) (if (stringp el) el (format "%S" el)))
                                results "\n"))))))

(defvar melpaca-current-test nil)
(eval-and-compile
  (defmacro melpaca-deftest (arglist &rest body)
    "Run test thunk with ARGLIST and BODY."
    (declare (indent 1) (debug t))
    `(lambda (pr)
       (cl-letf ((melpaca-current-test (melpaca-test ,@arglist))
                 ((symbol-function 'warn) #'format))
         (condition-case err
             (when-let ((output ,(macroexp-progn body)))
               (push output (melpaca-test-output melpaca-current-test)))
           ((error) (push (format (cadr err) (cddr err)) (melpaca-test-errors melpaca-current-test))))
         (melpaca-print-test melpaca-current-test)
         (not (and (melpaca-test-required melpaca-current-test)
                   (melpaca-test-errors melpaca-current-test)))))))

(defcustom melpaca-test-functions
  (list
   (melpaca-deftest (:title "PR recipe parses" :required t :syntax 'emacs-lisp)
     (or (alist-get 'melpaca-recipe pr) (error "Unable to parse recipe")))
   (melpaca-deftest (:title "Submission contains 1 recipe")
     (or (= (alist-get 'changed_files pr) 1)
         (error "Please submit a single recipe per pull request")))
   (melpaca-deftest (:title "Package recipe valid" :required t)
     (melpaca-validate-recipe (alist-get 'melpaca-recipe pr)))
   (melpaca-deftest (:title "Package upstream reachable")
     (melpaca--validate-upstream-url (alist-get 'url (alist-get 'melpaca-info pr))))
   (melpaca-deftest (:title "Package installs" :required t :syntax 'emacs-lisp)
     (let* ((recipe (alist-get 'melpaca-recipe pr))
            (id (car recipe)))
       (elpaca-try recipe)
       (elpaca-wait)
       (when-let ((e (elpaca-get id))
                  ((eq (elpaca--status e) 'failed))
                  (info (nth 2 (car (elpaca<-log e)))))
         (error "%s" info))))
   (melpaca-deftest (:title "Package compiles cleanly" :syntax 'emacs-lisp)
     (let* ((id (car (alist-get 'melpaca-recipe pr)))
            (pkg (symbol-name id))
            (regexp (concat "^" pkg)))
       (with-current-buffer (elpaca-log (concat regexp " | byte-comp | warn\\|error ") t)
         (cl-loop for entry in tabulated-list-entries
                  do (push (string-trim (aref (cadr entry) 2))
                           (melpaca-test-errors melpaca-current-test))))))
   (melpaca-deftest  (:title "Package satisfies package-lint" :syntax 'emacs-lisp)
     (let* ((e (elpaca-get (car (alist-get 'melpaca-recipe pr))))
            (main (elpaca<-main e))
            (repo (elpaca<-repo-dir e)))
       (melpaca--init-package-lint)
       (setf (melpaca-test-title melpaca-current-test)
             (replace-regexp-in-string "Package" (melpaca-test-title melpaca-current-test)
                                       main))
       (declare-function package-lint-buffer "package-lint")
       (cl-loop for (line col type message) in
                (package-lint-buffer (find-file-noselect (expand-file-name main repo)))
                do (push (format "%d:%d %s %s\n" type line col message)
                         (melpaca-test-errors melpaca-current-test))))))
  "List of tests to run in test sub-process. Each is called with a PR alist."
  :type '(list function))

(defvar melpaca-generic-fetchers '(git hg))
(defvar melpaca-fetchers (append melpaca-generic-fetchers '(github gitlab codeberg sourcehut)))
(defvar melpaca-poll-interval 0.1)
(defvar melpaca-test-timeout 60)
(defvar melpaca--blocking nil)
(defvar melpaca--location (or load-file-name (buffer-file-name)))

(defun melpaca--validate-id (object)
  "Signal error if OBJECT is not a valid MELPA recipe ID."
  (let ((id (or (car-safe object) (error "Reicpe must be a non-nil list"))))
    (unless (symbolp id) (error "Recipe ID %S must be a symbol" id))
    (unless (> (length (symbol-name id)) 2)
      (warn "Recipe ID %s should be longer than 2 characters" id))
    (when (elpaca-menu-item id) ;;@TODO: hg type support in menu
      (warn "Recipe ID %s already present in MELPA archive" id))
    (when (alist-get id package--builtin-versions)
      (warn "Recipe ID %s shadows built-in Emacs package" id))
    (when-let ((repo (plist-get :repo (cdr object)))
               (name (cadr (split-string repo "/" )))
               ((not (equal (file-name-sans-extension name) (symbol-name id)))))
      (warn "Recipe ID %s does not match :repo name %s" id name))))

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
  (cl-destructuring-bind
      (&key fetcher url repo commit branch version-regexp files old-names)
      (or (cdr object) (error "No :fetcher"))
    (melpaca--validate-id object)
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
      (when (eq fetcher 'hg) (warn ":fetcher hg ignores :commit %s" commit))
      (unless (stringp commit) (error ":commit must be a string")))
    (when branch
      ;;@TODO: :branch valid for :fetcher hg?
      (unless (stringp branch) (error ":branch must be a string")))
    (when version-regexp
      (warn ":version-regexp rarely necessary")
      (unless (stringp version-regexp) (error ":verson-regexp must be a string")))
    (when files
      (warn ":files rarely necessary")
      (melpaca--validate-files files))
    (when old-names (unless (and (listp old-names) (cl-every #'symbolp old-names))
                      (error ":old-names must be a list of symbols")))))

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
  (when (or (null url) (string-empty-p url)) (error "No URL found in first comment"))
  (with-current-buffer (url-retrieve-synchronously url 'silent t 30)
    (unless (equal url-http-response-status 200)
      (error "%S returned statsus %S" url url-http-response-status))))

(defun melpaca--display-results ()
  "Display test results."
  (pop-to-buffer (get-buffer-create "*melpaca*") '((display-buffer-reuse-window))))

(defun melpaca--diff-to-recipe (url)
  "Return recipe from diff URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (unless (equal url-http-response-status 200)
      (error "Unable to download %S %S" url url-http-response-status))
    (goto-char (point-max))
    (backward-list)
    (read (current-buffer))))

(defun melpaca--parse-pr-post (body)
  "Return plist of form (:description DESCRIPTION :url URL) from PR BODY."
  (setq body (replace-regexp-in-string "\\(?:<!-+?[^z-a]*?->\\)" "" body)) ; Strip comments
  (cl-mapcar #'cons melpaca-pr-post-sections
             (mapcar #'string-trim
                     (split-string body melpaca-markdown-section-regexp 'omit-nulls))))

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
      (let ((pr (json-parse-buffer :object-type 'alist :null-object nil :false-object nil)))
        (when-let ((err (alist-get 'message pr))) (error "Github API Response: %s" err))
        (push (cons 'melpaca-recipe (melpaca--diff-to-recipe (alist-get 'diff_url pr))) pr)
        (push (cons 'melpaca-info (melpaca--parse-pr-post (alist-get 'body pr))) pr)
        pr))))

(cl-defun melpaca (number) ;;@TODO make test sequence user option
  "Provide feedback for Pull Request with NUMBER."
  (with-current-buffer (get-buffer-create "*melpaca*")
    (let* ((pr (melpaca-pull-request number))
           (melpaca--blocking t)
           (elpaca-test-finish-functions (lambda (&rest _) (setq melpaca--blocking nil)))
           (standard-output (current-buffer)))
      (eval `(elpaca-test
               :ref local
               :buffer "*melpaca*"
               :timeout ,melpaca-test-timeout
               :early-init (setq elpaca-menu-functions '(elpaca-menu-melpa))
               :init
               (load ,melpaca--location nil 'nomessage) ;;Load this version of melpaca
               (let ((melpaca-test-functions ',melpaca-test-functions)
                     (melpaca-test-status-indicators ',melpaca-test-status-indicators)
                     (melpaca-generic-fetchers ',melpaca-generic-fetchers)
                     (melpaca-fetchers ',melpaca-fetchers)
                     (melpaca-poll-interval ,melpaca-poll-interval)
                     (melpaca-test-timeout ,melpaca-test-timeout))
                 (run-hook-with-args-until-failure 'melpaca-test-functions ',pr)))
            t)
      (while melpaca--blocking (sit-for melpaca-poll-interval))))
  (funcall melpaca-after-test-function))

;;@TODO: Report git-blamed :old-names incorrect element type string
;;@TODO: License Check
;;@TODO: Extra feedback?
;;@TODO: Previous art suggestions

(provide 'melpaca)
;;; melpaca.el ends here
