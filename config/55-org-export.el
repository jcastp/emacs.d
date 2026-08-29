(with-eval-after-load 'ox
;; -*- lexical-binding: t; -*-
) ; with-eval-after-load ox

(with-eval-after-load 'ox
;;(load (expand-file-name "vendor/ox-extra/ox-extra.el" my-data-dir))
(use-package org-contrib
  :ensure t
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
) ; with-eval-after-load ox

(with-eval-after-load 'ox
;; don't show the "validate" link on org-html exports
(setq org-html-validation-link nil)
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(use-package htmlize
  :ensure t)
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(require 'ox-md)
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Add smart quotes for latex
  (org-export-with-smart-quotes t)
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("ews"
     "\\documentclass[11pt, twoside, hidelinks]{memoir}
    \\setstocksize{9.25in}{7.5in}
    \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
    \\setlrmarginsandblock{2cm}{1cm}{*} 
    \\setulmarginsandblock{1.5cm}{2.25cm}{*}
    \\checkandfixthelayout
    \\setcounter{tocdepth}{0}
    \\OnehalfSpacing
    \\usepackage{ebgaramond}
    \\usepackage[htt]{hyphenat}
    \\chapterstyle{bianchi}
    \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
    \\setsubsecheadstyle{\\normalfont \\raggedright \\textbf}
    \\setsubsubsecheadstyle{\\normalfont\\centering}
    \\renewcommand\\texttt[1]{{\\normalfont\\fontfamily{cmvtt}
      \\selectfont #1}}
    \\usepackage[font={small, it}]{caption}
    \\pagestyle{myheadings}
    \\usepackage{ccicons}
    \\usepackage[authoryear]{natbib}
    \\bibliographystyle{apalike}
    \\usepackage{svg}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(defvar my/latex-spanish-preamble
  "\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[spanish]{babel}
\\usepackage{graphicx}
\\usepackage{todonotes}
\\usepackage[normalem]{ulem}
\\usepackage{hyperref}
\\usepackage{parskip}
\\usepackage{fourier}
\\newcommand{\\fin}{\\plainbreak*{3}}
\\newcommand{\\edit}[1]{\\todo[inline]{#1}}
\\newcommand{\\adendo}[1]{\\todo[size=\\tiny]{#1}}"
  "Shared LaTeX preamble for Spanish memoir/reporting classes.")

(defun my/add-memoir-class (name &optional draft chapter-only)
  "Add memoir LaTeX class with NAME.
When DRAFT is non-nil, enable draft mode.
When CHAPTER-ONLY is non-nil, start with chapters (no parts)."
  (let* ((draft-option (if draft ",draft" ",final"))
         (levels (if chapter-only
                     ;; Chapter-only: no \\part level
                     '(("\\chapter{%s}" . "\\chapter*{%s}")
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       ("\\paragraph{%s}" . "\\paragraph*{%s}")
                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                   ;; Standard: includes \\part level
                   '(("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
         (preamble (concat
                    (format "\\documentclass[a4paper,17pt%s,openright,twoside]{memoir}\n"
                            draft-option)
                    my/latex-spanish-preamble
                    "
\\usepackage{minted}

%% Chapter style
\\chapterstyle{dash}

%% How the page is formatted
\\pagestyle{Ruled}

%% change the paragraph spacing
\\setlength{\\parskip}{0.2\\baselineskip}

               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]")))
    (add-to-list 'org-latex-classes
                 `(,name ,preamble ,@levels))))

(with-eval-after-load 'ox-latex
  (my/add-memoir-class "memoir" nil nil)           ; Standard with parts
  (my/add-memoir-class "memoir_draft" t nil)       ; Draft with parts
  (my/add-memoir-class "memoir_chapter" nil t)     ; Final, chapter-only
  (my/add-memoir-class "memoir_chapter_draft" t t)) ; Draft, chapter-only
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               `("reporting"
                 ,(concat "\\documentclass[a4paper,17pt,openright,twoside]{memoir}\n"
                          my/latex-spanish-preamble
                          "

%% How the page is formatted
\\pagestyle{plain}

               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]")
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(require 'ox-org)
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(require 'ox-beamer)
(require 'ox-latex)
(setq org-export-allow-bind-keywords t)
(setq org-latex-listings 'minted)
) ; with-eval-after-load ox

(with-eval-after-load 'ox
;; load the gnuplot package
(use-package gnuplot)

(add-to-list 'org-latex-packages-alist '("" "minted"))
(org-babel-do-load-languages 'org-babel-load-languages '(
                                                         (shell . t)
                                                         (python . t)
                                                         (C . t)
                                                         (ruby . t)
                                                         (js . t)
                                                         (ditaa . t)
                                                         (gnuplot . t)
  							 (plantuml . t)
  							 (dot . t)))
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :after ox)
) ; with-eval-after-load ox

(defun my/blog-capture-template ()
  "Return an ox-hugo subtree capture template for a new blog draft."
  (let* ((title (read-from-minibuffer "Post title: "))
         (slug  (replace-regexp-in-string
                 "[^a-z0-9]+" "-"
                 (downcase (string-trim title)))))
    (mapconcat #'identity
               `(,(concat "** TODO " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " slug)
                 ,(concat ":EXPORT_DATE: " (format-time-string "%Y-%m-%d"))
                 ":END:"
                 "%?\n")
               "\n")))

(add-to-list 'org-capture-templates
             '("B" "Blog draft" entry
               (file+olp "~/Nextcloud/personal/hugo_blog/content-org/all-posts.org"
                         "drafts")
               (function my/blog-capture-template)
               :jump-to-captured t
               :empty-lines-before 1)
             t)

(with-eval-after-load 'ox
(use-package ox-epub
  :ensure t)
) ; with-eval-after-load ox

(with-eval-after-load 'ox
(use-package ox-pandoc
  :ensure t)
) ; with-eval-after-load ox
