;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-04-06 14:52:22>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-recentf-project/emacs-recentf-project.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; emacs-recentf-project.el --- Project-based recentf management -*- lexical-binding: t -*-

;; Author: Yusuke Watanabe
;; Keywords: files, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/yourusername/emacs-recentf-project

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides project-based management for recentf files.
;; It allows you to maintain separate recentf lists for different projects,
;; as well as a global view that combines all project files.

;;; Code:

(require 'recentf)

;; Variables
;; ----------------------------------------

(defgroup emacs-recentf-project nil
  "Project-based recentf management."
  :group 'files
  :prefix "emacs-recentf-project-")

(defcustom emacs-recentf-project-list '("global")
  "List of project names for recentf configuration."
  :type '(repeat string)
  :group 'emacs-recentf-project)

(defcustom emacs-recentf-project-global-name "global"
  "Project name for the combined global recentf view."
  :type 'string
  :group 'emacs-recentf-project)

(defvar emacs-recentf-project-current "global"
  "Currently active recentf project configuration.")

;; Functions
;; ----------------------------------------

(defun emacs-recentf-project-file (project-name)
  "Return the path to the recentf file for PROJECT-NAME."
  (if (string= project-name emacs-recentf-project-global-name)
      (expand-file-name ".recentf" user-emacs-directory)
    (expand-file-name (format ".recentf-%s" project-name)
                      user-emacs-directory)))

(defun emacs-recentf-project-get-available ()
  "Get list of all available recentf projects including existing files."
  (let ((project-files
         (directory-files user-emacs-directory nil
                          "^\\.recentf-\\(.+\\)$"))
        (projects (copy-sequence emacs-recentf-project-list)))
    (dolist (file project-files)
      (when (string-match "^\\.recentf-\\(.+\\)$" file)
        (let ((project-name (match-string 1 file)))
          (unless (member project-name projects)
            (push project-name projects)))))
    projects))

(defun emacs-recentf-project-update-global ()
  "Update the global recentf file with entries from all project recentf files."
  (interactive)
  ;; Save current state
  (let ((current-project emacs-recentf-project-current)
        (current-recentf-list recentf-list)
        (global-list '()))

    ;; Collect all files from all projects
    (dolist (project (emacs-recentf-project-get-available))
      (unless (string= project emacs-recentf-project-global-name)
        (let ((project-file (emacs-recentf-project-file project)))
          (when (file-exists-p project-file)
            ;; Temporarily load this project's recentf
            (let ((temp-recentf-list nil))
              (load project-file)
              ;; Append to global list (avoiding duplicates)
              (dolist (file recentf-list)
                (unless (member file global-list)
                  (push file global-list))))))))

    ;; Sort the global list by access time (most recent first)
    (setq global-list (sort global-list
                            (lambda (a b)
                              (let
                                  ((a-time
                                    (or
                                     (and (file-exists-p a)
                                          (nth 5 (file-attributes a)))
                                     '(0 0)))
                                   (b-time
                                    (or
                                     (and (file-exists-p b)
                                          (nth 5 (file-attributes b)))
                                     '(0 0))))
                                (time-less-p b-time a-time)))))

    ;; Save the combined list to the global recentf file
    (let ((recentf-list global-list)
          (recentf-save-file
           (emacs-recentf-project-file
            emacs-recentf-project-global-name)))
      (recentf-save-list))

    ;; Restore current state
    (setq emacs-recentf-project-current current-project
          recentf-list current-recentf-list)))

(defun emacs-recentf-project-switch (project-name)
  "Switch to recentf list for PROJECT-NAME."
  (interactive
   (list (completing-read "Switch to recentf project: "
                          (emacs-recentf-project-get-available)
                          nil nil nil nil
                          emacs-recentf-project-current)))

  ;; Check if project is new and ask for confirmation
  (when (and (not (member project-name emacs-recentf-project-list))
             (not
              (file-exists-p (emacs-recentf-project-file project-name)))
             (not
              (y-or-n-p
               (format "Project '%s' does not exist. Create it? "
                       project-name))))
    (user-error "Project creation cancelled"))

  ;; Add project to list if not already there
  (unless (member project-name emacs-recentf-project-list)
    (add-to-list 'emacs-recentf-project-list project-name))

  ;; Save current recentf list
  (when recentf-mode
    (recentf-save-list))

  ;; Update recentf-save-file to the project-specific file
  (setq recentf-save-file (emacs-recentf-project-file project-name))

  ;; Create the file if it doesn't exist or load the existing one
  (if (file-exists-p recentf-save-file)
      ;; File exists, load it
      (recentf-load-list)
    ;; File doesn't exist, create it with empty recentf-list
    (with-temp-file recentf-save-file
      (insert
       (format ";;; Recentf list for project: %s\n" project-name))
      (insert "(setq recentf-list '())\n"))
    ;; Reset recentf-list to empty list explicitly
    (setq recentf-list nil))

  ;; Update current project name
  (setq emacs-recentf-project-current project-name)
  (message "Switched to recentf list for project: %s%s"
           project-name
           (if recentf-list "" " (empty list)")))

(defun emacs-recentf-project-add (project-name)
  "Add a new PROJECT-NAME to the list of recentf projects."
  (interactive "sProject name: ")
  (unless (member project-name emacs-recentf-project-list)
    (setq emacs-recentf-project-list
          (append emacs-recentf-project-list (list project-name)))
    (message "Added project %s to recentf projects" project-name)))

(defun emacs-recentf-project-switch-from-dialog ()
  "Switch to a different recentf project in the recentf dialog."
  (interactive)
  (let ((project-name (completing-read "Switch to recentf project: "
                                       (emacs-recentf-project-get-available)
                                       nil nil nil nil
                                       emacs-recentf-project-current)))
    (emacs-recentf-project-switch project-name)
    ;; Refresh the recentf dialog
    (recentf-cancel-dialog)
    (recentf-open-files)))

(defun emacs-recentf-project-switch-to-global ()
  "Switch to the global recentf view that combines all projects."
  (interactive)
  ;; First update the global recentf file
  (emacs-recentf-project-update-global)
  ;; Then switch to it
  (emacs-recentf-project-switch emacs-recentf-project-global-name))

(defun emacs-recentf-project-dispatch ()
  "Dispatch to appropriate recentf function based on context."
  (interactive)
  (cond
   ((eq major-mode 'recentf-dialog-mode)
    (let ((project (completing-read "Switch to project: "
                                    (emacs-recentf-project-get-available)
                                    nil t nil nil
                                    emacs-recentf-project-current)))
      ;; Special handling for global project
      (when (string= project emacs-recentf-project-global-name)
        (emacs-recentf-project-update-global))
      ;; Switch to the selected project
      (emacs-recentf-project-switch project)
      ;; Refresh the dialog
      (recentf-cancel-dialog)
      (recentf-open-files)))
   (t
    (recentf-open-files))))

(defun emacs-recentf-project-setup ()
  "Set up emacs-recentf-project package."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (define-key-after (lookup-key global-map [menu-bar tools])
    [emacs-recentf-projects]
    '(menu-item "Recent Files by Project"
                emacs-recentf-project-dispatch
                :help "Open recent files by project")
    'recentf)
  (global-set-key (kbd "C-c C-r") 'emacs-recentf-project-dispatch))

;;;###autoload

(define-minor-mode emacs-recentf-project-mode
  "Toggle emacs-recentf-project mode.
When enabled, recentf will maintain separate lists for different projects."
  :global t
  :lighter " RecentfP"
  :group 'emacs-recentf-project
  (if emacs-recentf-project-mode
      (emacs-recentf-project-setup)
    (global-set-key (kbd "C-c C-r") 'recentf-open-files)))

;;; emacs-recentf-project.el ends here

(provide 'emacs-recentf-project)

(when
    (not load-file-name)
  (message "emacs-recentf-project.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))