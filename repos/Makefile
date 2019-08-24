MAKEFILE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
ZSHRC_GEN := ~/.zshrc_gen

.PHONY: install
install: install-fzf

.PHONY: install-fzf
install-fzf:
	#cd $(MAKEFILE_DIR)fzf && ./install --bin
	if [ grep "PATH:$$PATH:$(abspath $(MAKEFILE_DIR)fzf/bin)" "$(ZSHRC_GEN)" ]; then \
		echo ok; \
	else \
		echo "PATH:$$PATH:$(abspath $(MAKEFILE_DIR)fzf/bin)" >> "$(ZSHRC_GEN)";
	fi
