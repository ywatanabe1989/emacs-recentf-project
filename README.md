<!-- ---
!-- Timestamp: 2025-04-06 14:57:18
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-recentf-project/README.md
!-- --- -->

# emacs-recentf-project

Project-based recentf management for Emacs.

## Overview

`emacs-recentf-project` allows you to maintain separate recentf lists for different projects, as well as a global view that combines all project files. This is particularly useful when working with multiple projects that might have files with similar names or paths.

## Installation

### Manual

Clone this repository:

```sh
git clone https://github.com/yourusername/emacs-recentf-project.git ~/.emacs.d/lisp/emacs-recentf-project
```

Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-recentf-project")
(require 'emacs-recentf-project)
(emacs-recentf-project-mode 1)
```

### Using straight.el

```elisp
(straight-use-package
 '(emacs-recentf-project :type git :host github :repo "yourusername/emacs-recentf-project"))
(emacs-recentf-project-mode 1)
```

### Using use-package

```elisp
(use-package emacs-recentf-project
  :ensure t
  :config
  (emacs-recentf-project-mode 1))
```

## Usage

Once enabled, you can use:

- `C-c C-r` to open the recentf dialog with project switching capabilities
- `M-x emacs-recentf-project-switch` to explicitly switch between projects
- `M-x emacs-recentf-project-add` to add a new project

In the recentf dialog, you can switch between projects by selecting from the available project list.

## Configuration

```elisp
;; Customize the list of known projects (global is always included)
(setq emacs-recentf-project-list '("global" "project1" "project2"))

;; Change the name of the global project view (default is "global")
(setq emacs-recentf-project-global-name "all")

;; Custom keybinding if desired
(global-set-key (kbd "C-c r") 'emacs-recentf-project-dispatch)
```

## How it works

The package creates separate recentf files for each project:
- `.recentf` for the global view
- `.recentf-project1`, `.recentf-project2`, etc. for project-specific views

When you switch projects, the package automatically loads the appropriate recentf list and saves the current one.

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

<!-- EOF -->