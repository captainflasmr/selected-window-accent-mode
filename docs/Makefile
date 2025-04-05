# Set default values if not specified
PREFIX ?= /usr
infodir ?= $(PREFIX)/share/info

.PHONY: docs install-docs

# Build the info documentation
docs: selected-window-accent-mode.info

selected-window-accent-mode.info: selected-window-accent-mode.texi
	makeinfo --no-split $< -o $@

# Install the info documentation
install-docs: docs
	@echo "Installing info documentation..."
	install -d $(DESTDIR)$(infodir)
	install -m 644 selected-window-accent-mode.info $(DESTDIR)$(infodir)/
	install-info --info-dir=$(DESTDIR)$(infodir) selected-window-accent-mode.info
