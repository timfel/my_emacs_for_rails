;;; -*- lexical-binding: t -*-
(require 'use-package)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package timfel
  :demand
  :config
  (ignore-errors
    (require 'orcl))
  (add-to-list 'load-path (expand-file-name "lisp" timfel/gist-location)))

(use-package emacs-ci
  :after timfel
  :commands ci-dashboard
  :functions (magit-current-section
              org-link-set-parameters
              org-link-store-props
              timfel/org-follow-ci-link
              timfel/org-store-ci-link)
  :defines ci-dashboard-base-url
  :config
  (with-eval-after-load 'org
    (defun timfel/org-store-ci-link ()
      (when-let* ((section (magit-current-section))
                  (typ (oref section type))
                  (val (oref section value))
                  (url (cond ((eq typ 'ci-dashboard-job)
                              (alist-get 'url val))
                             ((eq typ 'ci-pr-entry)
                              (when-let* ((mrg (alist-get 'mergeJob val))
                                          (prv (or (alist-get 'pullRequest val) (alist-get 'pullRequest mrg)))
                                          (to-ref (alist-get 'toRef prv))
                                          (to-repo (alist-get 'repository to-ref))
                                          (to-slug (alist-get 'slug to-repo))
                                          (pr-id (alist-get 'id prv)))
                                (format "bitbucket:projects/G/repos/%s/pull-requests/%s" to-slug pr-id))))))
        (org-link-store-props :type "bitbucket" :link url :description url)
        url))

    (defun timfel/org-follow-ci-link (url)
      (browse-url (format "%s/%s" ci-dashboard-base-url url)))

    (org-link-set-parameters
     "bitbucket"
     :follow #'timfel/org-follow-ci-link
     :store #'timfel/org-store-ci-link)))

(use-package presentation
  :commands (presentation-mode)
  :custom
  (presentation-default-text-scale 4))

(use-package pypytrace-mode
  :commands pypytrace-mode)

(use-package javap-handler)

(use-package redo+
  :bind (("C--" . redo)))
