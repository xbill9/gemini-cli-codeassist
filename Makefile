SUBDIRS := backend-rust \
	battle-royale \
	cloudrun-rust \
	firestore-cli-rust \
	firestore-client-rust \
	firestore-cloudrun-rust \
	firestore-https-c \
	firestore-https-csharp \
	firestore-https-flutter \
	firestore-https-go \
	firestore-https-haskell \
	firestore-https-java \
	firestore-https-kotlin \
	firestore-https-lisp \
	firestore-https-perl \
	firestore-https-php \
	firestore-https-python \
	firestore-https-ruby \
	firestore-https-rust \
	firestore-https-swift \
	firestore-https-ts \
	firestore-stdio-c \
	firestore-stdio-cplus \
	firestore-stdio-csharp \
	firestore-stdio-flutter \
	firestore-stdio-go \
	firestore-stdio-haskell \
	firestore-stdio-java \
	firestore-stdio-kotlin \
	firestore-stdio-lisp \
	firestore-stdio-perl \
	firestore-stdio-php \
	firestore-stdio-python \
	firestore-stdio-ruby \
	firestore-stdio-rust \
	firestore-stdio-swift \
	firestore-stdio-ts \
	gcp-client-rust \
	gcp-cloudrun-client-rust \
	gcp-https-client-rust \
	gcp-stdio-client-rust \
	hello-rust \
	log-rust \
	logging-client-rust \
	mcp-cli-rust \
	mcp-client-rust \
	mcp-cloudrun-rust \
	mcp-https-c \
	mcp-https-cplus \
	mcp-https-csharp \
	mcp-https-flutter \
	mcp-https-go \
	mcp-https-haskell \
	mcp-https-java \
	mcp-https-kotlin \
	mcp-https-lisp \
	mcp-https-perl \
	mcp-https-php \
	mcp-https-python \
	mcp-https-ruby \
	mcp-https-rust \
	mcp-https-swift \
	mcp-https-ts \
	mcp-rust \
	mcp-stdio-c \
	mcp-stdio-cplus \
	mcp-stdio-csharp \
	mcp-stdio-flutter \
	mcp-stdio-go \
	mcp-stdio-haskell \
	mcp-stdio-java \
	mcp-stdio-kotlin \
	mcp-stdio-lisp \
	mcp-stdio-perl \
	mcp-stdio-php \
	mcp-stdio-python \
	mcp-stdio-ruby \
	mcp-stdio-rust \
	mcp-stdio-swift \
	mcp-stdio-ts \
	mcp-ts \
	mcptest-rust \
	pubsub-client-rust \
	stdio-rust \
	weather-rust

.PHONY: list clean release $(addprefix clean-,$(SUBDIRS)) $(addprefix release-,$(SUBDIRS))

list:
	@echo "Subdirectories:"
	@for dir in $(SUBDIRS); do \
		echo "  $$dir"; \
	done

clean: $(addprefix clean-,$(SUBDIRS))

release: $(addprefix release-,$(SUBDIRS))

define clean_task
clean-$(1):
	@echo "----------------------------------------------------------------"
	@echo "Cleaning $(1)..."
	@if [ -f "$(1)/Makefile" ]; then \
		$(MAKE) -C $(1) clean || echo "Make clean failed or not defined in $(1), continuing..."; \
	elif [ -f "$(1)/Cargo.toml" ]; then \
		echo "Detected Rust project. Running cargo clean..."; \
		(cd $(1) && cargo clean); \
	elif [ -f "$(1)/go.mod" ]; then \
		echo "Detected Go project. Running go clean..."; \
		(cd $(1) && go clean); \
	elif [ -f "$(1)/package.json" ]; then \
		echo "Detected Node.js project. Removing node_modules and dist..."; \
		rm -rf $(1)/node_modules $(1)/dist; \
	else \
		echo "No build system detected for $(1). Skipping."; \
	fi
endef

define release_task
release-$(1):
	@echo "----------------------------------------------------------------"
	@echo "Releasing $(1)..."
	@if [ -f "$(1)/Makefile" ]; then \
		$(MAKE) -C $(1) release || echo "Make release failed or not defined in $(1), continuing..."; \
	elif [ -f "$(1)/Cargo.toml" ]; then \
		echo "Detected Rust project. Running cargo build --release..."; \
		(cd $(1) && cargo build --release); \
	elif [ -f "$(1)/go.mod" ]; then \
		echo "Detected Go project. Running go build..."; \
		(cd $(1) && go build -v ./...); \
	elif [ -f "$(1)/package.json" ]; then \
		echo "Detected Node.js project. Running npm install && npm run build..."; \
		(cd $(1) && npm install && npm run build); \
	else \
		echo "No build system detected for $(1). Skipping."; \
	fi
endef

$(foreach dir,$(SUBDIRS),$(eval $(call clean_task,$(dir))))
$(foreach dir,$(SUBDIRS),$(eval $(call release_task,$(dir))))
