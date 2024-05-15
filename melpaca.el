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

(defcustom melpaca-pr-post-sections '(:description :url :associations)
  "List of PR post sections."
  :type '(list symbol))

(defcustom melpaca-test-status-indicators '((error . "❌") (warning . "⚠️") (pass . "✅"))
  "Alist of form ((STATUS . STRING)...)."
  :type 'alist)

(defvar melpaca--test-output nil)
(eval-and-compile
  (defmacro melpaca-deftest (arglist &rest body)
    "Run test thunk with ARGLIST and BODY."
    (declare (indent 1) (debug t))
    (let ((testsym (make-symbol "test")))
      `(lambda (pr)
         (let* ((,testsym (melpaca--test ,@arglist))
                (melpaca--test-output nil))
           (condition-case err
               (when-let ((output ,(macroexp-progn body)))
                 (push (cons 'pass output) melpaca--test-output))
             ((error) (melpaca-error "%S" err)))
           (melpaca-print-test (melpaca--test-title ,testsym)
                               (melpaca--test-syntax ,testsym)
                               melpaca--test-output)
           (not (and (melpaca--test-required ,testsym)
                     (eq (melpaca-test-status) 'error))))))))

(defun melpaca-recipe (pr)
  "Return recipe from PR alist."
  (alist-get 'recipe (alist-get 'melpaca pr)))

(defcustom melpaca-test-functions
  (list
   (melpaca-deftest (:title "PR recipe parses" :required t :syntax 'emacs-lisp)
     (or (melpaca-recipe pr) (error "Unable to parse recipe")))
   (melpaca-deftest (:title "Submission contains 1 recipe" :required t)
     (or (= (alist-get 'changed_files pr) 1)
         (error "Please submit a single recipe per pull request")))
   (melpaca-deftest (:title "Package recipe valid" :required t)
     (melpaca-validate-recipe (melpaca-recipe pr)))
   (melpaca-deftest (:title "Package upstream reachable" :required t)
     (melpaca--validate-upstream-url (alist-get 'info (alist-get 'melpaca pr))))
   (melpaca-deftest (:title "Package installs" :required t :syntax 'emacs-lisp)
     (let* ((recipe (melpaca-recipe pr))
            (id (car recipe)))
       (elpaca-try recipe)
       (elpaca-wait)
       (when-let ((e (elpaca-get id))
                  ((eq (elpaca--status e) 'failed))
                  (info (nth 2 (car (elpaca<-log e)))))
         (melpaca-error "%s" info))))
   (melpaca-deftest (:title "Package compiles cleanly" :syntax 'emacs-lisp)
     (let* ((id (car (melpaca-recipe pr)))
            (pkg (symbol-name id))
            (regexp (concat "^" pkg)))
       (with-current-buffer (elpaca-log (concat regexp " | byte-comp | Warning\\|Error") t)
         (setq melpaca--test-output
               (cl-loop for entry in tabulated-list-entries
                        for info = (aref (cadr entry) 2)
                        collect (cons (if (string-match-p "Warning" info) 'warning 'error)
                                      info))))))
   (progn
     (declare-function package-lint-buffer "package-lint")
     (melpaca-deftest  (:title "Package satisfies package-lint" :syntax 'emacs-lisp)
       (let* ((e (elpaca-get (car (melpaca-recipe pr))))
              (main (elpaca<-main e)))
         (find-file (expand-file-name main (elpaca<-repo-dir e)))
         (melpaca--init-package-lint)
         (cl-loop for (line col type message) in (package-lint-buffer)
                  collect (cons type (format "%s:%s %s" line col message)))))))
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
      (melpaca-warn "Recipe ID %s should be longer than 2 characters" id))
    (when (elpaca-menu-item id) ;;@TODO: hg type support in menu
      (melpaca-warn "Recipe ID %s already present in MELPA archive" id))
    (when (alist-get id package--builtin-versions)
      (melpaca-warn "Recipe ID %s shadows built-in Emacs package" id))
    (when-let ((repo (plist-get :repo (cdr object)))
               (name (cadr (split-string repo "/" )))
               ((not (equal (file-name-sans-extension name) (symbol-name id)))))
      (melpaca-warn "Recipe ID %s does not match :repo name %s" id name))))

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
      (when (eq fetcher 'hg) (melpaca-warn ":fetcher hg ignores :commit %s" commit))
      (unless (stringp commit) (error ":commit must be a string")))
    (when branch
      ;;@TODO: :branch valid for :fetcher hg?
      (unless (stringp branch) (error ":branch must be a string")))
    (when version-regexp
      (melpaca-warn ":version-regexp rarely necessary")
      (unless (stringp version-regexp) (error ":verson-regexp must be a string")))
    (when files
      (melpaca-warn ":files rarely necessary")
      (melpaca--validate-files files))
    (when old-names (unless (and (listp old-names) (cl-every #'symbolp old-names))
                      (error ":old-names must be a list of symbols")))))

(defun melpaca-test-status (&optional results)
  "Return overall status of test RESULTS."
  (unless results (setq results melpaca--test-output))
  (cond ((assoc 'error melpaca--test-output) 'error)
        ((assoc 'warning melpaca--test-output) 'warning)
        (t 'pass)))

(defun melpaca-print-test (heading type results)
  "Print HEADING, test RESULTS of TYPE."
  (let* ((status (melpaca-test-status results))
         (passp (eq status 'pass)))
    (princ (concat "\n"
                   (concat "<details" (unless passp " open") "><summary>")
                   (alist-get status melpaca-test-status-indicators) " " heading
                   (unless passp  "</summary>")
                   "\n\n"))
    (princ (format "\n```%s\n%s\n```\n</details>\n"
                   (or type "")
                   (mapconcat (lambda (r) (format "%s" (cdr r))) results "\n")))))

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
  "Display test results."
  (pop-to-buffer (get-buffer-create "*melpaca*") '((display-buffer-reuse-window))))


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

(defun melpaca--parse-pr-post (body)
  "Return plist of form (:description DESCRIPTION :url URL) from PR BODY."
  (cl-mapcar #'list melpaca-pr-post-sections
             (mapcar #'string-trim
                     (split-string body melpaca-markdown-section-regexp 'omit-nulls))))

(cl-defstruct (melpaca--test (:constructor melpaca--test) (:copier nil) (:named))
  "Test struct."
  title required syntax)

(defun melpaca-test-output (type info &rest args)
  "Push TYPE INFO formatted with ARGS to `melpaca--test-output'."
  (push (cons type (apply #'format info args)) melpaca--test-output))

(defun melpaca-error (info &rest args)
  "Fail test with INFO ARGS."
  (apply #'melpaca-test-output 'error info args))

(defun melpaca-warn (info &rest args)
  "Warn with INFO ARGS."
  (apply #'melpaca-test-output 'warning info args))

(cl-defun melpaca (number) ;;@TODO make test sequence user option
  "Provide feedback for Pull Request with NUMBER."
  (with-current-buffer (get-buffer-create "*melpaca*")
    (let* ((pr (melpaca-pull-request number))
           (melpaca--blocking t)
           (elpaca-test-start-functions nil)
           (elpaca-test-finish-functions (lambda (&rest _) (setq melpaca--blocking nil)))
           (standard-output (current-buffer)))
      (when-let ((err (alist-get 'message pr))) (error "Github API Response: %s" err))
      (push (cons 'melpaca
                  (list (cons 'recipe (melpaca--diff-to-recipe (alist-get 'diff_url pr)))
                        (cons 'info (melpaca--parse-pr-post (alist-get 'body pr)))))
            pr)
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
