# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration using **literate programming** via Org-mode. Configuration is written in `.org` files and tangled to `.el` files by `org-babel-load-file` at startup.

## Architecture

### Boot sequence

1. `early-init.el` — Disables `package-enable-at-startup`, loads `custom.el`, removes GUI chrome (menu/tool/scroll bars), disables splash screen
2. `init.el` — Sets up MELPA, `use-package` (with `always-ensure t`), defines machine/environment predicates, then loads the main config via `org-babel-load-file`
3. `emacs-config.org` — **Main shared configuration**: packages, keybindings, UI, org-mode, programming, etc.
4. `emacs-config-personal.org` — Loaded only when `my-homeenvironment-p` is true. Covers personal org-agenda, org-roam, writing tools (org-scribe), elfeed, mastodon, dashboard, themes, fontaine presets
5. `emacs-config-work.org` — Loaded only when `my-workenvironment-p` is true (env var `WORKING=WORK`). Overrides agenda files, tags, todo keywords, fontaine presets, and capture templates

### Key design patterns

- **Environment-conditional loading**: The config uses predicates (`my-homeenvironment-p`, `my-workenvironment-p`, `my-worksystem-p`, `my-desktopsystem-p`, `my-writinglaptop-p`) to load different features per machine/environment. The `WORKING` env var controls home vs work.
- **Data directory separation**: `my-config-dir` (`~/.emacs.d/`) is the committed config; `my-data-dir` (`~/Nextcloud/config/.emacs.d/`) holds personal data (bookmarks, dictionaries, backups) that is NOT committed.
- **Encrypted directory**: `my-clear-directory` points to a gocryptfs-mounted directory; `init.el` checks if it's mounted and sets `my-clear-directory-is-mounted-p`. Features depending on it (mastodon, org-journal) are gated on this predicate.
- **Org blocks with `:tangle no`**: Disabled or experimental features use `:tangle no` to prevent them from being loaded. The "Testing" heading in each org file is the staging area for code not yet promoted to the main config.
- **fontaine is environment-specific**: `(use-package fontaine :ensure t)` lives in the personal and work configs, not the shared config. Presets in each env are defined independently; the work config resets them completely.

### Custom keymap (`C-q`)

`C-q` is bound to `my/key-prefix-map` with sub-keymaps and direct bindings:
- `a` — AI (gptel) sub-map
- `b` — buffers sub-map (`o` → org-scratch)
- `c` — `my/centered-mode`
- `i` — `tempel-insert`
- `o` — online sub-map (`e` → eww, `f` → elfeed, `m` → mastodon)
- `q` — direct file access sub-map (`c` → config, `k` → keys, `t` → tasks, `e` → escritura)
- `r` — `consult-recent-file`
- `t` — themes/fonts sub-map (`f` → fontaine-set-preset, `t` → consult-theme, `v` → visual-fill-column-mode)
- `u` — `consult-outline`
- `w` — writing sub-map (org-scribe modes, word count, synonyms, exercises)

### Important directories

- `templates/` — Tempel snippet templates (not committed but referenced)
- `roamtemplates/` — Org-roam capture templates (BookNote, CharacterIdea, WritingIdea)
- `elpa/` — Package directory (gitignored)

## Editing conventions

- The primary editing target is the `.org` files — never edit `emacs-config.el`, `emacs-config-personal.el`, or `emacs-config-work.el` directly (they are tangled output)
- Packages are configured with `use-package` (`:ensure t` is the global default)
- `custom.el` is managed by the Emacs customize system and `package-vc-selected-packages` — edit with care
- Spanish keyboard layout is assumed (e.g., `C-ñ` for expand-region, Spanish electric pairs `¡!` `¿?`)
- Spelling uses `hunspell` with multi-dictionary `en_US,es_ES`
- Custom packages from Codeberg: `org-scribe`, `org-scribe-planner`, `org-context-extended` (installed via `package-vc-selected-packages` in `custom.el`)

### Naming conventions

- **Variables**: `my-` prefix (e.g., `my-config-dir`, `my-data-dir`)
- **Functions**: `my/` prefix (e.g., `my/reload-config`, `my/centered-mode`)
- **Predicates**: end with `-p` (e.g., `my-worksystem-p`, `my-homeenvironment-p`)

### Things to watch out for

- **Work config is additive for most things, but resets some**: `org-capture-templates` is explicitly reset to `'()` before adding work templates. `fontaine-presets` is completely redefined, losing any personal presets.
- **Testing sections are tangled by default**: Org headings named "Testing" do not automatically get `:tangle no`. Each src block inside must be explicitly marked `:tangle no` if not ready for use.
- **`my-writing-project` in Testing** needs to be set to the real project subdirectory name before the `writing/start-session` function is enabled.

## Testing changes

There is no test suite. To verify changes:

- **Inside Emacs**: `C-c r r` (calls `my/reload-config`, which reloads `init.el`)
- **Headless check for errors**: `emacs --batch --debug-init --eval "(kill-emacs)"`
- **Tangle an org file manually**: `emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "emacs-config.org")'`

