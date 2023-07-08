;;; misskey-test.el --- ERT test for misskey.el       -*- lexical-binding: t; -*-

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

(ert-deftest misskey-test/json/walk-test/do-nothing-for-nonexist-key ()
  "Do nothing if given KEY isn't in OBJ"
  (should (equal (misskey/json/walk '() :nonexist 'id)
	  '()))
  (should (equal (misskey/json/walk '(:foo "a") :nonexist 'id)
	  '(:foo "a"))))

(ert-deftest misskey-test/json/walk-test/keep-unspecified-keys-unchanged ()
  "Keep unspecified keys as is"
  (should (equal (misskey/json/walk '(:foo 1 :bar 2 :baz 3) :foo '1+)
	  '(:foo 2 :bar 2 :baz 3)))
  (should (equal (misskey/json/walk '(:foo 1 :bar 2 :baz 3) :baz '1+)
	  '(:foo 1 :bar 2 :baz 4))))

(ert-deftest misskey-test/json/walks-test ()
  "walks should apply each KEY & FUNC accordingly"
  (equal (misskey/json/walks '(:foo 1 :bar 2 :baz 3) :foo '1+ :baz '1+)
	 '(:foo 2 :bar 2 :baz 4)))

(ert-deftest misskey-test/json-read-test/createdAt-conversion ()
  ":createdAt should be \"decoded\" by `decode-time'"
  (should
   (equal (with-temp-buffer
	    (insert (json-encode '(:createdAt "2023-05-31T12:08:47Z")))
	    (goto-char (point-min))
	    (misskey/json-read))
	  '(:createdAt (47 8 12 31 5 2023 nil nil 0)))))

(ert-deftest misskey-test/json-read-test/updatedAt-conversion ()
  ":updatedAt should be \"decoded\" by `decode-time'"
  (should
   (equal (with-temp-buffer
	    (insert (json-encode '(:updatedAt "2023-05-31T12:08:47Z")))
	    (goto-char (point-min))
	    (misskey/json-read))
	  '(:updatedAt (47 8 12 31 5 2023 nil nil 0)))))


(ert-deftest misskey-test/envs/get/not-found-should-return-nil ()
  "If required env is not registered, it should return nil"
  (let ((misskey/envs (misskey/envs/empty)))
    (should (null (misskey/envs/get "foo" "example.com")))))

(ert-deftest misskey-test/envs/get/if-found-return-env ()
  "If required env is found, return it"
  (let ((misskey/envs (misskey/envs/empty))
	(env (make-misskey/misskeyEnv :host "example.com"
				      :token "dummy"
				      :username "foo")))
    (puthash "foo@example.com" env (plist-get misskey/envs :envs))
    (setf (plist-get misskey/envs :default) "foo@example.com")
    (should (equal (misskey/envs/get "foo" "example.com") env))))

(ert-deftest misskey-test/misskey-api/no-params ()
  "No :optional-params :required-params should create function with
only ENV argument, and pass `nil' as argument for 'misskey/call-deferred'"
  (let* ((expanded (macroexpand-1 '(misskey-api test-api)))
	 (expanded-func-arglist (elt expanded 2))
	 (expanded-call-deferred-third-arg (elt (elt (elt (elt expanded 4) 2) 1) 3)))
    (and (should (equal expanded-func-arglist '(env)))
	 (should (equal expanded-call-deferred-third-arg nil)))))

(ert-deftest misskey-test/misskey-api/required-params ()
  "`misskey-api' should define parameters given by :required-params as normal argument,
 and pass it to `misskey/call-deferred' as is"
  (let* ((expanded (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp)))))
	 (expanded-func-arglist (elt expanded 2))
	 (expanded-call-deferred-third-arg (elt (elt (elt (elt expanded 4) 2) 1) 3)))
    (and (should (equal expanded-func-arglist '(env foo)))
	 (should (equal (eval expanded-call-deferred-third-arg '((foo . "test"))) '(:foo "test"))))))


(ert-deftest misskey-test/misskey-api/optional-params ()
  ":optional-params should be defined by `&key', and should be applied to `misskey/call-deferred'"
  (let* ((expanded (macroexpand-1 '(misskey-api test-api :optional-params ((foo . stringp)))))
	 (expanded-func-arglist (elt expanded 2))
	 (expanded-call-deferred-third-arg (elt (elt (elt (elt expanded 4) 2) 1) 3))

	 ;; With multiple optional params
	 (multi-expanded (macroexpand-1 '(misskey-api test-api :optional-params ((foo . stringp) (bar . integerp)))))
	 (multi-expanded-func-arglist (elt multi-expanded 2))
	 (multi-expanded-call-deferred-third-arg (elt (elt (elt (elt multi-expanded 4) 2) 1) 3))
	 )
    (and (should (equal expanded-func-arglist '(env &key foo)))
	 (should (equal (eval expanded-call-deferred-third-arg '((foo . "foo"))) '(:foo "foo")))

	 (should (equal multi-expanded-func-arglist '(env &key foo bar)))
	 (should (equal (eval multi-expanded-call-deferred-third-arg '((foo . "foo") (bar . 1))) '(:foo "foo" :bar 1)))
	 ;; When optional-param value is NIL, it should not be given to `misskey/call-deferred'
	 (should (equal (eval multi-expanded-call-deferred-third-arg '((foo . "foo") (bar . nil))) '(:foo "foo"))))))

(ert-deftest misskey-test/misskey-api/optional-params/omit-if-nil ()
  "optional params should not be passed to `misskey/call-deferred' if it isn't passed to generated function"
  (let* ((expanded (macroexpand-1 '(misskey-api test-api :optional-params ((foo . stringp)))))
	 (expanded-func-arglist (elt expanded 2))
	 (expanded-call-deferred-third-arg (elt (elt (elt (elt expanded 4) 2) 1) 3)))
    (and (should (equal expanded-func-arglist '(env &key foo)))
	 (should (equal (eval expanded-call-deferred-third-arg '((foo . nil))) '())))))

(ert-deftest misskey-test/misskey-api/required-and-optional-params ()
  (let* ((expanded (macroexpand-1 '(misskey-api test-api :required-params ((foo . stringp)) :optional-params ((bar . integerp)))))
	 (expanded-func-arglist (elt expanded 2))
	 (expanded-call-deferred-third-arg (elt (elt (elt (elt expanded 4) 2) 1) 3)))
    (and (should (equal expanded-func-arglist '(env foo &key bar)))
	 (should (equal (eval expanded-call-deferred-third-arg '((foo . "bar") (bar . 1))) '(:foo "bar" :bar 1)))
	 (should (equal (eval expanded-call-deferred-third-arg '((foo . "bar") (bar . nil))) '(:foo "bar"))))))

(provide 'misskey-test)
;;; misskey-ert.el ends here
