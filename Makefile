SUBDIRS := backend-rust \
	cloudrun-rust \
	firestore-cli-rust \
	firestore-client-rust \
	firestore-cloudrun-rust \
	firestore-https-go \
	firestore-https-java \
	firestore-https-python \
	firestore-https-rust \
	firestore-https-ts \
	firestore-stdio-go \
	firestore-stdio-java \
	firestore-stdio-python \
	firestore-stdio-rust \
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
	mcp-https-go \
	mcp-https-java \
	mcp-https-python \
	mcp-https-ts \
	mcp-rust \
	mcp-stdio-go \
	mcp-stdio-java \
	mcp-stdio-python \
	mcp-stdio-rust \
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
