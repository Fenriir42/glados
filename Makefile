.PHONY: all
all: glados

SELF := $(firstword $(MAKEFILE_LIST))

glados: .build/mk.cli\:cli
	cp $(shell cabal -v0 list-bin exe:cli) $@
	chmod +x $@
	@ $(LOG_TIME) "Build $(C_CYAN)$(notdir $@)$(C_RESET)"

define mk-target
_name_$(strip $1) := $(subst \:library,,$(subst +,\:,$(strip $1)))

.PHONY: $$(_name_$(strip $1))

.build/mk.$$(_name_$(strip $1)): $$(shell git ls-files | grep ".*hs")
	$$(call cabal-cmd-$($(strip $1)), $$(_name_$(strip $1)))
	@ $$(LOG_TIME) "$$(C_CYAN)$$(_name_$(strip $1))$$(C_RESET)"
	@ touch $$@

$$(_name_$(strip $1)): .build/mk.$$(_name_$(strip $1))

endef

cabal-cmd-lib = cabal build $(strip $1)
cabal-cmd-exe = cabal build $(strip $1)
cabal-cmd-test = cabal test $(strip $1)

ifneq ($(MAKECMDGOALS),fclean)
CABAL-EXTRACT := $(shell cabal -v0 list-bin exe:cabal-extract --dry-run)

all-targets +=
-include .build/types.mk

# $(foreach target, $(all-targets), \
	$(info $(call mk-target, $(target))))

$(eval $(foreach target, $(all-targets), \
	$(eval $(call mk-target, $(target)))))
endif

.build/types.mk: .build/layout.json $(SELF)
	@ mkdir -p $(dir $@)
	@ jq -r '.[] | "\(.name)=\(.type)"' .build/layout.json | tr ':' '+' > $@
	@ grep -Po "^(.*)(?=[=])" $@ \
		| xargs -i echo "all-targets += {}" >> $@
	@ $(LOG_TIME) "Generated $(C_PURPLE)$@$(C_RESET)"

.build/layout.json: $(CABAL-EXTRACT)
	@ mkdir -p $(dir $@)
	@ cabal run cabal-extract -- . > $@

$(CABAL-EXTRACT): cabal-extract
	@ cabal build cabal-extract
	@ $(LOG_TIME) "Build $(C_CYAN)$(notdir $@)$(C_RESET)"

.PHONY: tests_run
tests_run:
	@ cabal test all

.PHONY: clean
clean:
	@ $(RM) .build/mk.*

.PHONY: fclean
test:
	cabal test all

coverage:
	cabal test --enable-coverage

fclean: clean
	@ cabal clean

.NOTPARALLEL: re
.PHONY: re
re: fclean all

# ---------------------------------------------------------------------------
# Install / uninstall / packaging

VERSION := 1.0.0
DESTDIR  ?=
PREFIX   ?= /usr/local

BIN_DEST   = $(DESTDIR)$(PREFIX)/bin
SHARE_DEST = $(DESTDIR)$(PREFIX)/share/quant/lib
MAN_DEST   = $(DESTDIR)$(PREFIX)/share/man/man1

.PHONY: install
install:
	@ cabal build cli glados-lsp glados-repl
	@ install -d $(BIN_DEST) $(SHARE_DEST) $(MAN_DEST)
	@ install -m 755 $(shell cabal -v0 list-bin exe:cli) $(BIN_DEST)/glados
	@ install -m 755 $(shell cabal -v0 list-bin exe:glados-lsp) $(BIN_DEST)/glados-lsp
	@ install -m 755 $(shell cabal -v0 list-bin exe:glados-repl) $(BIN_DEST)/glados-repl
	@ cp -r std/. $(SHARE_DEST)/
	@ install -m 644 man/glados.1 $(MAN_DEST)/glados.1
	@ install -m 644 man/glados-lsp.1 $(MAN_DEST)/glados-lsp.1
	@ install -m 644 man/glados-repl.1 $(MAN_DEST)/glados-repl.1
	@ install -m 644 man/quant-fmt.1 $(MAN_DEST)/quant-fmt.1
	@ install -m 644 man/wheatley.1 $(MAN_DEST)/wheatley.1
	@ install -m 644 man/quant.1 $(MAN_DEST)/quant.1
	@ $(LOG_TIME) "Install $(C_GREEN)glados$(C_RESET) -> $(BIN_DEST)/glados"
	@ $(LOG_TIME) "Install $(C_GREEN)glados-lsp$(C_RESET) -> $(BIN_DEST)/glados-lsp"
	@ $(LOG_TIME) "Install $(C_GREEN)glados-repl$(C_RESET) -> $(BIN_DEST)/glados-repl"
	@ $(LOG_TIME) "Install $(C_GREEN)stdlib$(C_RESET) -> $(SHARE_DEST)"
	@ $(LOG_TIME) "Install $(C_GREEN)man pages$(C_RESET) -> $(MAN_DEST)"

.PHONY: uninstall
uninstall:
	@ rm -f $(PREFIX)/bin/glados $(PREFIX)/bin/glados-lsp $(PREFIX)/bin/glados-repl
	@ rm -rf $(PREFIX)/share/quant
	@ rm -f $(PREFIX)/share/man/man1/glados.1
	@ rm -f $(PREFIX)/share/man/man1/glados-lsp.1
	@ rm -f $(PREFIX)/share/man/man1/glados-repl.1
	@ rm -f $(PREFIX)/share/man/man1/quant-fmt.1
	@ rm -f $(PREFIX)/share/man/man1/wheatley.1
	@ rm -f $(PREFIX)/share/man/man1/quant.1
	@ $(LOG_TIME) "Uninstall $(C_RED)glados$(C_RESET)"

.PHONY: deb
deb:
	@ rm -rf .deb-staging
	@ $(MAKE) install DESTDIR=.deb-staging PREFIX=/usr/local
	@ install -d .deb-staging/DEBIAN
	@ sed 's/VERSION/$(VERSION)/g' packaging/debian/control > .deb-staging/DEBIAN/control
	@ install -m 755 packaging/debian/postinst .deb-staging/DEBIAN/postinst
	@ dpkg-deb --build .deb-staging quant_$(VERSION)_amd64.deb
	@ rm -rf .deb-staging
	@ $(LOG_TIME) "Package $(C_CYAN)quant_$(VERSION)_amd64.deb$(C_RESET)"

ifneq ($(shell command -v tput),)
  ifneq ($(shell tput colors),0)

mk-color = \e[$(strip $1)m

C_BEGIN := \033[A
C_RESET := $(call mk-color, 00)

C_RED := $(call mk-color, 31)
C_GREEN := $(call mk-color, 32)
C_YELLOW := $(call mk-color, 33)
C_BLUE := $(call mk-color, 34)
C_PURPLE := $(call mk-color, 35)
C_CYAN := $(call mk-color, 36)

  endif
endif

NOW = $(shell date +%s%3N)

STIME := $(shell date +%s%3N)
export STIME

define TIME_MS
$$( expr \( $$(date +%s%3N) - $(STIME) \))
endef

BOXIFY = "[$(C_BLUE)$(1)$(C_RESET)] $(2)"

ifneq ($(shell command -v printf),)
  LOG_TIME = printf $(call BOXIFY, %6s , %b\n) "$(call TIME_MS)"
else
  LOG_TIME = echo -e $(call BOXIFY, $(call TIME_MS) ,)
endif
