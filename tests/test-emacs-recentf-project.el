;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-06 14:53:51>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-recentf-project/tests/test-emacs-recentf-project.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;; emacs-recentf-project-tests.el --- Tests for emacs-recentf-project -*- lexical-binding: t -*-

;; Copyright (C) 2025 Yusuke Watanabe

;; Author: Yusuke Watanabe
;; Keywords: files, convenience, test

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the emacs-recentf-project package.
;; Run with (ert-run-tests-interactively "^emacs-recentf-project-")

;;; Code:

(require 'ert)
(require 'emacs-recentf-project)

;; Helpers

(defun emacs-recentf-project-test-setup ()
  "Setup testing environment."
  ;; Save original values
  (setq emacs-recentf-project-test-original-list
        emacs-recentf-project-list
        emacs-recentf-project-test-original-current
        emacs-recentf-project-current
        emacs-recentf-project-test-original-global-name
        emacs-recentf-project-global-name
        emacs-recentf-project-test-original-save-file
        recentf-save-file)

  ;; Set up test values
  (setq emacs-recentf-project-list '("global" "test-project")
        emacs-recentf-project-current "global"
        recentf-list
        '("/path/to/test/file1.txt" "/path/to/test/file2.txt"))

  ;; Make sure recentf mode is on
  (unless recentf-mode
    (recentf-mode 1)))

(defun emacs-recentf-project-test-teardown ()
  "Restore original state after testing."
  ;; Restore original values
  (setq emacs-recentf-project-list
        emacs-recentf-project-test-original-list
        emacs-recentf-project-current
        emacs-recentf-project-test-original-current
        emacs-recentf-project-global-name
        emacs-recentf-project-test-original-global-name
        recentf-save-file
        emacs-recentf-project-test-original-save-file))

;; Tests
(ert-deftest emacs-recentf-project-project-file-test ()
  "Test project file name generation."
  (should (string= (emacs-recentf-project-file "global")
                   (expand-file-name ".recentf" user-emacs-directory)))
  (should (string= (emacs-recentf-project-file "test-project")
                   (expand-file-name ".recentf-test-project"
                                     user-emacs-directory))))

(ert-deftest emacs-recentf-project-add-test ()
  "Test adding a new project."
  (emacs-recentf-project-test-setup)
  (unwind-protect
      (progn
        (emacs-recentf-project-add "new-project")
        (should (member "new-project" emacs-recentf-project-list)))
    (emacs-recentf-project-test-teardown)))

(ert-deftest emacs-recentf-project-switch-test ()
  "Test switching between projects."
  (emacs-recentf-project-test-setup)
  (unwind-protect
      (let ((original-recentf-list recentf-list))
        ;; Prepare a fake test project file
        (let ((test-file (emacs-recentf-project-file "test-project")))
          (with-temp-file test-file
            (insert ";;; Recentf list for project: test-project\n")
            (insert
             "(setq recentf-list '(\"/test/project/file.txt\"))\n"))

          ;; Now switch to test project
          (emacs-recentf-project-switch "test-project")

          ;; Verify switch happened
          (should
           (string= emacs-recentf-project-current "test-project"))
          (should (string= recentf-save-file test-file))
          (should (equal recentf-list '("/test/project/file.txt")))

          ;; Switch back to global
          (emacs-recentf-project-switch "global")
          (should (string= emacs-recentf-project-current "global"))
          (should (equal recentf-list original-recentf-list))))
    (emacs-recentf-project-test-teardown)))

(ert-deftest emacs-recentf-project-mode-test ()
  "Test enabling and disabling the minor mode."
  (emacs-recentf-project-test-setup)
  (unwind-protect
      (progn
        ;; Test enabling
        (let ((orig-map (copy-keymap global-map)))
          (emacs-recentf-project-mode 1)
          (should emacs-recentf-project-mode)
          (should
           (eq (lookup-key global-map (kbd "C-c C-r"))
               'emacs-recentf-project-dispatch))

          ;; Test disabling
          (emacs-recentf-project-mode -1)
          (should (not emacs-recentf-project-mode))
          (should
           (eq (lookup-key global-map (kbd "C-c C-r"))
               'recentf-open-files))))
    (emacs-recentf-project-test-teardown)))

;;; emacs-recentf-project-tests.el ends here

(provide 'test-emacs-recentf-project)

(when
    (not load-file-name)
  (message "test-emacs-recentf-project.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))