;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu gptel-rewrite gptel-abort)
  :config
  (setq gptel-default-mode 'org-mode)

  ;; Custom directives (system prompts)
  (setq gptel-directives
	'((default     . "You are a helpful assistant.")
          (writing     . "You are a very good writing assistant. You are known for your excellent and on-point advices. Give honest advice, and help improve prose, suggest alternatives, and preserve the author's voice. Be concise and honest.")
          (coding      . "You are a coding assistant. Provide working, idiomatic code. Explain only when necessary.")
          (spanish     . "Eres un asistente en español. Responde siempre en español.")
          (proofreading . "Proofread the text. List only real errors (grammar, spelling, punctuation). Do not suggest style changes.")))

  ;; Local llama-swap backend (OpenAI-compatible API)
  (setq-default gptel-backend
                (gptel-make-openai "llama-swap"
                  :host "localhost:8080"
                  :protocol "http"
                  :stream t
                  :models '("llama3.2"
			      "gpt-oss"
			      "phi4-mini-4b-augmented"
			      "phi4-mini-4b"
			      "phi4-reasoning"
			      "qwen3.5-27B"
			      "qwen3-30b-VL")))

  (setq-default gptel-model "llama3.2"))

(defvar-keymap my/key-prefix-ai-map
  :doc "Functions related to AI assistance"
  "a" #'gptel            ;; open gptel chat buffer
  "s" #'gptel-send       ;; send region/buffer to LLM
  "m" #'gptel-menu       ;; transient menu for options
  "r" #'gptel-rewrite      ;; rewrite selected region in-place
  "x" #'gptel-abort)     ;; abort current request

(my/key-define "a" "AI" my/key-prefix-ai-map)
