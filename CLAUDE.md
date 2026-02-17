# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration using **literate programming** via Org-mode. Configuration is written in `.org` files and tangled to `.el` files by `org-babel-load-file` at startup.

## Architecture

### Boot sequence

1. `early-init.el` — Disables `package-enable-at-startup`, loads `custom.el`, removes GUI chrome (menu/tool/scroll bars), disables splash screen
2. `init.el` — Sets up MELPA, `use-package` (with `always-ensure t`), defines machine/environment predicates, then loads the main config via `org-babel-load-file`
3. `emacs-config.org` (~2160 lines) — **Main shared configuration**: packages, keybindings, UI, org-mode, programming, etc.
4. `emacs-config-personal.org` (~1810 lines) — Loaded only when `my-homeenvironment-p` is true. Covers personal org-agenda, org-roam, writing tools (org-scribe), elfeed, mastodon, dashboard, themes
5. `emacs-config-work.org` (~330 lines) — Loaded only when `my-workenvironment-p` is true (env var `WORKING=WORK`)

### Key design patterns

- **Environment-conditional loading**: The config uses predicates (`my-homeenvironment-p`, `my-workenvironment-p`, `my-worksystem-p`, `my-desktopsystem-p`, `my-writinglaptop-p`) to load different features per machine/environment. The `WORKING` env var controls home vs work.
- **Data directory separation**: `my-config-dir` (`~/.emacs.d/`) is the committed config; `my-data-dir` (`~/Nextcloud/config/.emacs.d/`) holds personal data (bookmarks, dictionaries, perspectives, backups) that is NOT committed.
- **Encrypted directory**: `my-clear-directory` points to a gocryptfs-mounted directory; init.el checks if it's mounted.
- **Custom keymap prefix**: `C-q` is bound to `my/key-prefix-map` with sub-keymaps for online (`o`), themes (`t`), buffers (`b`), direct file access (`q`), and writing (`w`).
- **Org blocks with `:tangle no`**: Disabled/canceled features use `:tangle no` to prevent them from being loaded.

### Important directories

- `templates/` — Tempel snippet templates (not committed but referenced)
- `roamtemplates/` — Org-roam capture templates (BookNote, CharacterIdea, WritingIdea)
- `writing/` — Writing-related org config
- `elpa/` — Package directory (gitignored)

## Editing conventions

- All `.el` files use `lexical-binding: t`
- Packages are configured with `use-package` (always-ensure is on globally, so `:ensure t` is default)
- The primary editing target is the `.org` files, not the generated `.el` files — never edit `emacs-config.el`, `emacs-config-personal.el`, or `emacs-config-work.el` directly
- `custom.el` is managed by Emacs customize system and `package-vc-selected-packages` — edit with care
- Spanish keyboard layout is assumed (e.g., `C-ñ` for expand-region, Spanish electric pairs `¡!` `¿?`)
- Spelling uses `hunspell` with multi-dictionary `en_US,es_ES`
- Custom packages from Codeberg: `org-scribe`, `org-scribe-planner`, `org-context-extended` (installed via `package-vc-selected-packages` in `custom.el`)

### Naming conventions

- **Variables**: `my-` prefix (e.g., `my-config-dir`, `my-data-dir`)
- **Functions**: `my/` prefix (e.g., `my/reload-config`, `my/centered-mode`)
- **Predicates**: end with `-p` (e.g., `my-worksystem-p`, `my-homeenvironment-p`)

## Testing changes

There is no test suite. To verify changes:

- **Inside Emacs**: `C-c r r` (calls `my/reload-config`, which reloads `early-init.el` and `init.el`)
- **Headless check for errors**: `emacs --batch --debug-init --eval "(kill-emacs)"`
- **Tangle an org file manually**: `emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "emacs-config.org")'`
