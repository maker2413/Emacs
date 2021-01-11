(provide 'init-packages)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package fish-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)
