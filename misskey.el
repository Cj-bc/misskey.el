;;; misskey.el --- API library for misskey       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cj-bc a.k.a Cj.bc_sd

;; Author:  <cj.bc_sd@outlook.jp>
;; Keywords: comm, lisp
;; Package-Requires: (request-deferred deferred)

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

;; 

;;; Code:

(require 'deferred)
(require 'request-deferred)
(require 'cl-macs)
(require 'seq)

(cl-defstruct misskey/misskeyEnv
  "Contains configurations for API call."
  (host :type string)
  (token :type string))

;;; ---- Internal functions

(defun misskey/json/walk (obj key func)
  "walk plist and apply FUNC for each occurrence of KEY.
It assumes OBJ is valid plist.


Idea behind this function is coming from pandoc library in Haskell.
Its document might helps you to understand concept of it:
https://hackage.haskell.org/package/pandoc-types-1.22.2.1/docs/Text-Pandoc-Walk.html


* Examples

```
(misskey/json/walk '(:id 0 :createdAt \"2022-01-01T12:30:10.00Z\"
			 :renote (:id 1 :createdAt  \"2022-01-01T12:30:10.00Z\"))
		   :createdAt '(lambda (d) (iso8601-parse d)))
>> (:id 0 :createdAt (10 30 12 1 1 2022 nil nil 0) :renote (:id 1 :createdAt (10 30 12 1 1 2022 nil nil 0)))
```
"
  (cond
   ((plistp obj)
    (when (plist-member obj key)
      (setq obj (plist-put (copy-sequence obj) key (funcall func (plist-get obj key)))))
    (seq-map `(lambda (item) (misskey/json/walk item ,key (function ,func))) obj))
   ((stringp obj) obj) ;; As string is 'sequence', I have to treat string before testing `sequencep'
   ((sequencep obj) (seq-map `(lambda (item) (misskey/json/walk item ,key (function ,func))) obj))
   (t obj)))

(defun misskey/json/walks (obj &rest keys)
  "`misskey/json/walk' wrapper to support multiple KEY FUNC pairs.

KEYS should be list, and elements are (KEY FUNC KEY2 FUNC2 KEY3 FUNC3...) for `misskey/json/walk'.
Therefore, length of KEYS should be multiple of 2. If not, it will return `nil'.

# Example:

(misskey/json/walks obj
  :foo a
  :bar b)

is equivalent to:

(misskey/json/walk
  (misskey/json/walk obj :foo a)
  :bar b)


# Real world example:

(misskey/json/walks obj
  :createdAt #'iso8601-parse
  :updatedAt #'iso8601-parse)

is equivalent to:

(misskey/json/walk
  (misskey/json/walk obj :createdAt #'iso8601-parse)
  #'iso8601-parse)
"
  (when (= (% (seq-length keys) 2) 0)
    (cl-loop for pair in (seq-partition keys 2)
  	  do (setq obj (misskey/json/walk obj (elt pair 0) (elt pair 1)))
  	  finally return obj)))

(defun misskey/json-read (&optional buf)
  "`json-read' with some modification for ease.

1. Convert timestamp string into Lisp timestamp"
  (with-current-buffer (or buf (current-buffer))
    (let* ((json-object-type 'plist)
	   (raw (json-read)))
      (misskey/json/walk (misskey/json/walk raw :createdAt #'iso8601-parse)
			 :updatedAt #'iso8601-parse))))

(defun misskey/debug/show-deferred-result (d)
  "[DEBUG-PURPOSE] print and display result of deffered-object D in new buffer.
It'll split frame to pop up that buffer

usage:
(misskey/debug/insert-deffered-result
 (misskey/api/users/show env '(:username \"cj_bc_sd\")))
→ will 
"
  (deferred:nextc d
    (lambda (result)
      (let ((buf (get-buffer-create
		  (generate-new-buffer-name "misskey/debug/show-deferred-result"))))
	(with-current-buffer buf
	  (emacs-lisp-mode)
	  (let ((print-length nil))
	    (insert (pp result))))
	(display-buffer buf 'display-buffer-in-side-window)))))

(defun misskey/call-deferred (env path body &optional credential-required)
  "General function to call api. This will return deferred object immediately, and run request asynchronously.

ENV should be `misskey/misskeyEnv' struct. PATH should be a string indicating relataive path of the API
endpoint(e.g. \"users/show\". BODY should be plist of valid request body for PATH."
  (request-deferred
   (format "https://%s/api/%s" (misskey/misskeyEnv-host env) path)
   :type "POST"
   :data (json-encode (if credential-required (plist-put body :i (misskey/misskeyEnv-token env)) body))
   :parser 'misskey/json-read
   :headers '(("Content-Type" . "application/json"))
   :error (cl-function (lambda (&key error-thrown &allow-other-keys)
  			 (message "misskey/call-deferred: (%s)" error-thrown)))))

(cl-defmacro misskey-api (path &key credential required-params optional-params)
  "Create misskey/PATH function as misskey-api endpoint.
PATH is symbol of API endpoint path, such as users/show or users/notes.

If API endpoint requires authentication, set CREDENTIAL non-nil.

REQUIRED-PARAMS represents list of required parameters and their validators.
There're two way to specify parameters: simple list or list following `anyOf'.

If endpoint requires all of given params, use simple list. If endpoint requires
one or more of given params (which is represented as 'anyOf' in misskey spec),
use `anyOf' followed by list.

Either case, element of list are (PARAMETER-NAME . VALIDATOR).

OPTIONAL-PARAMS represents list of optional parameters and their validators.
Optional parameters are defined as \"keyword arguments\" by using `cl-defun'.
Unlike REQUIRED-PARAMS, it is simple list, and element of list are
(PARAMETER-NAME . VALIDATOR). PARAMETER-NAME is directly used as keyword argument,
 so _do not_ make it keyword (by start with \":\").
"
  (let* ((path-str (symbol-name path))
	 (name-sym (intern (format "misskey/api/%s" path-str)))

	 ;; If `:required-params' starts with `anyOf' symbol, I have to receive required parameters as
	 ;; simple list and append some code to validate arguments
	 (is-required-arg-anyOf (and (sequencep required-params) (eq (car required-params) 'anyOf)))
	 (required-args (cond ((null required-params) '())
			      (is-required-arg-anyOf (seq-map #'car (cdr required-params)))
			      (t (seq-map #'car required-params))))
	 (required-arg-valiator
	  (cond ((null required-params) '(t))
		(is-required-arg-anyOf `((or ,@(seq-map '(lambda (x) `(,(cdr x) ,(car x))) (cdr required-params)))))
		(t (seq-map '(lambda (x) `(,(cdr x) ,(car x))) required-params))))
	 (optional-args
	  (when optional-params (seq-map #'car optional-params)))
	 (optional-arg-validator
	  (if (null optional-args) '(t)
	    (seq-map '(lambda (x) `(or (null ,(car x)) (,(cdr x) ,(car x)))) optional-params)))
	 (request-body (when (or required-args optional-params)
			 (let ((keys (seq-map
				      '(lambda (name)
					 `(list ,(intern (format ":%s" (symbol-name name))) ,name))
				      `(,@required-args ,@optional-args))
				))
			 `(seq-reduce
			   (lambda (acc name)
			     (if (seq-elt name 1) `(,@acc ,@name) acc))
			   (list ,@keys) '())))))
    `(cl-defun ,name-sym (env ,@required-args ,@(when optional-args `(&key ,@optional-args)))
       ;; Even though I can remove 'when' clause technically when no argument is given,
       ;; I intentionally not to do so because that would make code more complicated.
       ;; As a result, generated code sometimes have something like:
       ;;
       ;; (when t t (deferred:$ ...))
       (when (and ,@required-arg-valiator ,@optional-arg-validator)
	 (deferred:$
	     (misskey/call-deferred env ,path-str ,request-body ,credential)
	     (deferred:nextc it 'request-response-data))))))


;;; Predicates

(defun misskey-visibility-p (object)
  "Return t if OBJECT is valid string represents visibility.
Valid strings are:

- \"public\"
- \"home\"
- \"followers\"
- \"specified\"
"
  (and (stringp object)
   (or (string-equal object "public")
       (string-equal object "home")
       (string-equal object "followers")
       (string-equal object "specified"))))

(defun misskey-id-p (object)
  "Return t if OBJECT is valid misskey id.

Official: https://github.com/misskey-dev/misskey/blob/develop/packages/backend/src/models/id.ts
"
  (and (stringp object)
       (length= object 32)))

(defun misskey-sequence-of-id-p (object)
  "Return t if OBJECT is sequence of valid misskey id "
  (and (sequencep object)
       (seq-every-p 'misskey-id-p object)))

;;; API caller functions

(misskey-api users/show
	     :credential nil
	     :required-params
	     (anyOf (userId . stringp)
		    (userIds . misskey-sequence-of-id-p)
		    (username . stringp)))

(misskey-api notes/local-timeline :credential nil)
(misskey-api notes/create :credential t
	     :required-params
	     (anyOf
	      (text . stringp)
	      (fileIds . misskey-sequence-of-id-p)
	      (mediaIds . misskey-sequence-of-id-p)
	      (renoteId . stringp))
	     :optional-params
	     ((visibility . misskey-visibility-p)
	      (visibleUserIds . misskey-sequence-of-id-p)
	      (cw . stringp) (localOnly . booleanp) (noExtractMentions . booleanp)
	      (noExtractHashtags . booleanp) (noExtractEmojis . booleanp)
	      (replyId . stringp) (channelId . stringp)))

(misskey-api users/notes
	     :credential nil
	     :required-params
	     ((userId . stringp))
	     :optional-params
	     ((includeReplies . booleanp)
	      (limit . integerp)))


(provide 'misskey)
;;; misskey.el ends here
