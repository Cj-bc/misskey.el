;;; misskey.ert.el --- ERT test for misskey.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cj-bc a.k.a Cj.bc_sd

;; Author:  cj.bc-sd@outlook.jp
;; Keywords: 

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
(require 'ert)
(require 'misskey)
(require 'deferred)

(ert-deftest misskey-api-test-no-params ()
  "No :optional-params :required-params should give `nil' as body"
  (should (equal (macroexpand-1 '(misskey-api test-api))
		 '(cl-defun misskey/api/test-api (env)
		    (when (and t t)
		      (deferred:$
			(misskey/call-deferred env "test-api" nil nil)
			(deferred:nextc it 'request-response-data)))))))

(ert-deftest misskey-api-test/required-params ()
  (should (equal (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp))))
		 '(cl-defun misskey/api/test-api (env foo)
		    (when (and (stringp foo) t)
		      (deferred:$
			(misskey/call-deferred env "test-api"
					       (seq-reduce (lambda (acc name)
							      (if (seq-elt name 1) `(,@acc ,@name) acc))
							   (list (list :foo foo)) '()) nil)
			(deferred:nextc it 'request-response-data))))
		 ))
  (should (equal (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp) (bar . integerp))))
		 '(cl-defun misskey/api/test-api (env foo bar)
		    (when (and (stringp foo) (integerp bar) t)
		      (deferred:$
			(misskey/call-deferred env "test-api"
					       (seq-reduce (lambda (acc name)
							      (if (seq-elt name 1) `(,@acc ,@name) acc))
							   (list (list :foo foo) (list :bar bar)) '()) nil)
			(deferred:nextc it 'request-response-data))))
		 )))


(ert-deftest misskey-api-test/optional-params ()
  ""
  (should (equal (macroexpand-1 '(misskey-api test-api :optional-params ((foo . stringp))))
		 '(cl-defun misskey/api/test-api (env &key foo)
		    (when (and t (or (null foo) (stringp foo)))
		      (deferred:$
			(misskey/call-deferred env "test-api"
					       (seq-reduce (lambda (acc name)
							      (if (seq-elt name 1) `(,@acc ,@name) acc))
							   (list (list :foo foo)) '()) nil)
			(deferred:nextc it 'request-response-data))))
		 ))
  (should (equal (macroexpand-1 '(misskey-api test-api :optional-params ((foo . stringp) (bar . integerp))))
		 '(cl-defun misskey/api/test-api (env &key foo bar)
		    (when (and t (or (null foo) (stringp foo)) (or (null bar) (integerp bar)))
		      (deferred:$
			(misskey/call-deferred env "test-api"
					       (seq-reduce (lambda (acc name)
							      (if (seq-elt name 1) `(,@acc ,@name) acc))
							   (list (list :foo foo) (list :bar bar)) '()) nil)
			(deferred:nextc it 'request-response-data))))
		 )))

(ert-deftest misskey-api-test/required-and-optional-params ()
  (should (equal (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp)) :optional-params ((bar . integerp))))
		 '(cl-defun misskey/api/test-api (env foo &key bar)
		    (when (and (stringp foo) (or (null bar) (integerp bar)))
		      (deferred:$
			(misskey/call-deferred env "test-api"
					       (seq-reduce (lambda (acc name)
							     (if (seq-elt name 1) `(,@acc ,@name) acc))
							   (list (list :foo foo) (list :bar bar)) '()) nil)
			(deferred:nextc it 'request-response-data))))
		 ))
  (should (equal (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp) (baz . numberp)) :optional-params ((bar . integerp))))
		 '(cl-defun misskey/api/test-api (env foo baz &key bar)
		    (when (and (stringp foo) (numberp baz) (or (null bar) (integerp bar)))
		      (deferred:$
		  	(misskey/call-deferred env "test-api"
		  			       (seq-reduce (lambda (acc name)
		  					     (if (seq-elt name 1) `(,@acc ,@name) acc))
		  					   (list (list :foo foo) (list :baz baz) (list :bar bar)) '()) nil)
		  	(deferred:nextc it 'request-response-data))))
		 ))
  (should (equal (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp) ) :optional-params ((bar . integerp) (baz . numberp))))
		 '(cl-defun misskey/api/test-api (env foo &key bar baz)
		    (when (and (stringp foo)  (or (null bar) (integerp bar)) (or (null baz) (numberp baz)))
		      (deferred:$
		  	(misskey/call-deferred env "test-api"
		  			       (seq-reduce (lambda (acc name)
		  					     (if (seq-elt name 1) `(,@acc ,@name) acc))
		  					   (list (list :foo foo) (list :bar bar) (list :baz baz)) '()) nil)
		  	(deferred:nextc it 'request-response-data))))
		 )))

(provide 'misskey.ert)
;;; misskey.ert.el ends here
