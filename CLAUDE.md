# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration using **literate programming** via Org-mode. Configuration is written in `.org` files under `config/` and tangled to `.el` files by `org-babel-load-file` at startup.

The config is split by **domain** (one module per subject), not by environment. Environment differences — home vs work — are gated *inside* the module that owns them, so a feature and its per-environment variations sit together.

**Never** commit anything by yourself. The user will do it.

## Architecture

### Boot sequence

1. `early-init.el` — Disables `package-enable-at-startup`, loads `custom.el`, defers GC, removes GUI chrome (menu/tool/scroll bars), disables splash screen
2. `init.el` — Sets up MELPA and `use-package` (`always-ensure t`), defines the machine/environment predicates, then loads each enabled module from `my-config-modules`
3. `config/*.org` — The modules themselves

### The module list

`init.el` holds `my-config-modules`, an ordered alist of `(path . condition)`. **This is the load plan**: reading it top to bottom tells you exactly what loads and when. Numbering is spaced so new modules slot in without renumbering.

|module              |condition           |holds                                                                                |
|--------------------|--------------------|-------------------------------------------------------------------------------------|
|`00-core`           |always              |editing defaults, encoding, backups, state files, **the `C-q` keymap infrastructure**|
|`05-identity`       |always              |who I am + the `my-*-dir` path variables                                             |
|`10-ui`             |always              |frame, faces, modeline, `hl-line` colours                                            |
|`15-theme`          |always              |theme packages, light/dark rotation, all fontaine presets                            |
|`17-org-style`      |always              |org's appearance: heading ramp, drawers, metadata, org-modern, org-appear            |
|`20-completion`     |always              |vertico, consult, corfu, cape, orderless, embark                                     |
|`30-navigation`     |always              |windows, perspectives, avy, imenu-list, speedbar                                     |
|`40-org`            |always              |org core + each environment's todo keywords, tags, captures                          |
|`45-agenda`         |always              |both environments' agenda frames                                                     |
|`46-agenda-personal`|home                |personal agenda commands                                                             |
|`47-agenda-work`    |work                |work agenda commands, org-ql views, 1:1 tooling                                      |
|`50-org-roam`       |always              |roam, its UI, capture templates (shared DB, both environments)                       |
|`55-org-export`     |home                |every exporter, behind `with-eval-after-load 'ox`                                    |
|`60-writing`        |home                |org-scribe, tempel, writeroom, story files, org-journal                              |
|`70-prog`           |always              |magit, diff-hl, flymake, eglot, treesit, languages                                   |
|`80-apps`           |always              |eww, dired, elfeed, nov, calibre, pdf, eshell, mastodon, dashboard, jira             |
|`85-ai`             |`(home full-system)`|gptel                                                                                |
|`90-keymap`         |always              |the `C-q q` direct-file-access sub-map                                               |
|`99-scratch`        |*not loaded*        |staging area; tangles nothing                                                        |

Conditions understood by `my/module-enabled-p`: `always`, `home`, `work`, `full-system`, `clear` (encrypted dir mounted), or a **list**, meaning all of them must hold.

### Key design patterns

- **Domain modules, environment gated inline.** A per-environment difference lives next to what it modifies, as `(when my-homeenvironment-p ...)` / `(when my-workenvironment-p ...)`, not in a separate file.
- **Environment predicates**: `my-homeenvironment-p` / `my-workenvironment-p` come from the `WORKING` env var (`WORKING=WORK` selects work).
- **Machine predicates**: `my-worksystem-p`, `my-desktopsystem-p`, `my-writinglaptop-p` match hostnames. `my-full-system-p` is `(not my-writinglaptop-p)` — **deliberately negative**, because the hostname lists are closed sets and an unlisted machine should get the full config rather than silently lose features. It gates install footprint (`:ensure` installs even when deferred), not startup time.
- **Data directory separation**: `my-config-dir` (`~/.emacs.d/`) is the committed config; `my-data-dir` (`~/Nextcloud/config/.emacs.d/`) holds state (bookmarks, history, backups) and is NOT committed.
- **Per-environment state files**: `my/state-file` in `00-core` suffixes recentf/savehist/bookmarks with `-work` at work, so the two instances never overwrite each other's history.
- **Encrypted directory**: `my-clear-directory` is a gocryptfs mount; `init.el` sets `my-clear-directory-is-mounted-p`. Mastodon and org-journal are gated on it.
- **Paths come from variables**: `my-nextcloud-dir`, `my-agenda-dir`, `my-roam-dir`, `my-escritura-dir`, `my-blog-dir` in `05-identity`. Prefer these over literal `~/Nextcloud/...` strings.

### Custom keymap (`C-q`)

`C-q` is bound to `my/key-prefix-map`, created **empty** in `00-core`. Each module registers its own entries as it loads:

```elisp
(my/key-define "c" "centered mode" #'my/centered-mode)      ; single binding
(let ((m (my/key-define-submap "t" "themes and fonts")))    ; sub-map
  (my/key-define "f" "font preset" #'fontaine-set-preset m))
```

**To add a binding, edit the module that owns the command** — never collect it centrally. A module that does not load simply contributes nothing, so no `fboundp`/`boundp` guards are needed.

Current entries: `a` AI · `b` buffers · `c` centered-mode · `d` daily agenda · `i` tempel-insert · `o` online · `p` open report (work) · `q` direct file access · `r` recent file · `s` org styling · `t` themes/fonts · `u` outline · `w` writing

Note `my/key-define-submap` builds a **fresh** keymap, so two modules must never
register the same letter — the second call silently discards the first module's
bindings.

### Important directories

- `config/` — the modules (the editing target)
- `templates/` — Tempel snippets
- `roamtemplates/` — Org-roam capture templates
- `elpa/` — packages (gitignored)

## Editing conventions

- The editing target is `config/*.org` — **never edit `config/*.el`**, they are tangled output and gitignored
- Packages use `use-package` (`:ensure t` is the global default)
- `custom.el` is managed by the customize system — edit with care, and note it is **gitignored**, so it is not recoverable from git
- Spanish keyboard layout is assumed (`C-ñ` for expand-region, `¡!` `¿?` electric pairs)
- Spelling is **jinx** (`00-core`), which goes through **Enchant**, not hunspell directly — Enchant picks the provider per language (hunspell/aspell are both installed). The default is English only (`jinx-languages` is `"en"`); a file that is Spanish declares it with `#+language: es` or a file-local `jinx-languages`
- Codeberg packages are declared with `:vc` in `60-writing`: `org-scribe`, `org-context-extended`, `org-tracktable`

### Naming conventions

- **Variables**: `my-` prefix (`my-config-dir`, `my-full-system-p`)
- **Functions**: `my/` prefix (`my/reload-config`, `my/key-define`)
- **Predicates**: end in `-p` (`my-worksystem-p`, `my-homeenvironment-p`)

## Things to watch out for

- **Stale tangled output.** `org-babel-load-file` only re-tangles when the `.org` is *newer* than the `.el`. A stale `.el` silently shadows your edit, and you will debug a file that is not running. **Run `rm -f config/*.el` before any measurement or verification.**
- **Never `rm *.el` in the config root.** That glob matches `init.el`, `early-init.el` and `custom.el`, not just tangled output. Only ever `rm -f config/*.el`.
- **Deferral moves `:config`, so bindings must not live there.** A key bound with `global-set-key` inside `:config` never gets bound if the package is deferred; use `:bind`. An `auto-mode-alist` entry added in `:config` never fires; use `:mode`. Settings that must apply before load go in `:init`.
- **`:ensure` installs even when deferred.** Deferring saves startup time, not disk. Use `my-full-system-p` to keep heavy packages off the writing laptops.
- **Work resets capture templates.** `40-org` sets `org-capture-templates` to `'()` before adding the work ones. The two environments' blocks are adjacent so this is visible.
- **The agenda frames look parallel but are not.** Work does *not* skip DONE items (`org-agenda-skip-scheduled-if-done` is nil there) and the two category-icon alists differ. `45-agenda` documents what work lacks. Do not merge them without asking.
- **`99-scratch.org` sets `:tangle no` at the file level.** Code there ships only if a block deliberately overrides it. Put experiments there, not in a live module.
- **`jinx-languages` also drives in-buffer completion.** `20-completion` sets `cape-dict-file` to `my/cape-dict-files`, which picks the hunspell word list from the buffer's `jinx-languages`, so spellcheck and `cape-dict` never disagree about the language. It is called on every completion, so `C-M-$` retunes both at once. Adding a language means adding it to `my-hunspell-dictionaries` there, not just to `jinx-languages` — an unlisted language falls back to English rather than going silent.
- **org-scribe loads eagerly** (it calls `org-scribe-setup` at startup) and requires `ox`, which is why the exporters' `with-eval-after-load 'ox` wrapper currently saves nothing at home.

## Testing changes

There is no test suite. To verify changes:

- **Inside Emacs**: `C-c r r` (`my/reload-config`)
- **Headless, both environments** — the work path needs a larger frame or `persp-state-load` aborts:
  ```sh
  rm -f config/*.el
  WORKING=HOME emacs --batch --debug-init -l ./early-init.el -l ./init.el --eval '(kill-emacs)'
  WORKING=WORK emacs --batch --eval '(ignore-errors (set-frame-size (selected-frame) 220 70))' \
      -l ./early-init.el -l ./init.el --eval '(kill-emacs)'
  ```
- **Verify a refactor preserved behaviour**: snapshot runtime values (`org-capture-templates`, `org-agenda-files`, `fontaine-presets`, the resolved `hl-line` face, …) in both environments before and after, and diff. A structural change should come back byte-identical; anything else is a finding to explain, not to wave through.
- **Simulate a writing laptop**: `emacs --batch --eval '(setq my-writinglaptop-p t)' -l ./early-init.el -l ./init.el ...` — `defvar` will not override a value that is already set.
