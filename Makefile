# ==============================================================================
# NDXSQLite Makefile
# Build, test, install and cleanup
# ==============================================================================

.PHONY: all build build-sqlcipher build-examples rebuild \
        test test-sqlcipher test-all check \
        clean clean-src clean-lib clean-tests clean-examples clean-all \
        install uninstall package \
        help info tag

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

VERSION_FILE := src/ndxsqlite_version.inc
PACKAGE_FILE := src/NDXSQLite.lpk

# Detect number of CPU cores for parallel builds
NPROCS := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)

# Console examples directories (sorted)
CONSOLE_EXAMPLES := $(sort $(wildcard examples/console/*/))
GUI_EXAMPLES := $(sort $(wildcard examples/gui/*/))

# ==============================================================================
# HELP (Default target)
# ==============================================================================

help:
	@echo "==============================================================================="
	@echo "NDXSQLite Makefile"
	@echo "==============================================================================="
	@echo ""
	@echo "BUILD:"
	@echo "  make build            - Compile core tests"
	@echo "  make build-sqlcipher  - Compile SQLCipher tests"
	@echo "  make build-examples   - Compile all examples (159 console + 10 GUI)"
	@echo "  make build-all        - Compile everything"
	@echo "  make rebuild          - Clean and rebuild core tests"
	@echo ""
	@echo "TEST:"
	@echo "  make test             - Run core tests"
	@echo "  make test-sqlcipher   - Run SQLCipher tests (requires libsqlcipher)"
	@echo "  make test-all         - Run all tests including SQLCipher"
	@echo ""
	@echo "VERIFY:"
	@echo "  make check            - Full verification (clean, build, test)"
	@echo "  make info             - Show project information"
	@echo ""
	@echo "INSTALL:"
	@echo "  make install          - Install NDXSQLite package in Lazarus"
	@echo "  make uninstall        - Remove NDXSQLite package from Lazarus"
	@echo "  make package          - Build package without installing"
	@echo ""
	@echo "RELEASE:"
	@echo "  make tag VERSION=x.y.z - Update version and create git tag"
	@echo ""
	@echo "CLEANUP:"
	@echo "  make clean            - Clean src/ and tests/"
	@echo "  make clean-lib        - Clean only lib/ directories"
	@echo "  make clean-tests      - Clean tests/ (compiled + test DBs)"
	@echo "  make clean-examples   - Clean examples/"
	@echo "  make clean-all        - Clean everything"
	@echo ""
	@echo "==============================================================================="

# ==============================================================================
# BUILD TARGETS
# ==============================================================================

# Build core tests
build:
	@echo "==============================================================================="
	@echo "Building NDXSQLite tests..."
	@echo "==============================================================================="
	@echo ""
	@echo ">>> Compiling NDXSQLiteTests..."
	@cd tests && lazbuild NDXSQLiteTests.lpi
	@echo ""
	@echo ">>> Compiling NDXSQLiteAdvancedTests..."
	@cd tests && lazbuild NDXSQLiteAdvancedTests.lpi
	@echo ""
	@echo ">>> Compiling DateTimeUtilsTests..."
	@cd tests && lazbuild DateTimeUtilsTests.lpi
	@echo ""
	@echo ">>> Compiling DateTimeUtilsFunctionalTests..."
	@cd tests && lazbuild DateTimeUtilsFunctionalTests.lpi
	@echo ""
	@echo "[OK] Core build complete!"

# Build SQLCipher tests
build-sqlcipher:
	@echo "==============================================================================="
	@echo "Building SQLCipher tests..."
	@echo "==============================================================================="
	@echo ""
	@echo ">>> Compiling SQLCipherTests..."
	@cd tests && lazbuild SQLCipherTests.lpi
	@echo ""
	@echo "[OK] SQLCipher build complete!"

# Build all examples
build-examples:
	@echo "==============================================================================="
	@echo "Building all examples..."
	@echo "==============================================================================="
	@echo ""
	@echo ">>> Building console examples (149)..."
	@success=0; fail=0; \
	for dir in $(CONSOLE_EXAMPLES); do \
		name=$$(basename $$dir); \
		lpi=$$(find $$dir -maxdepth 1 -name "*.lpi" | head -1); \
		if [ -n "$$lpi" ]; then \
			if lazbuild "$$lpi" > /dev/null 2>&1; then \
				success=$$((success + 1)); \
			else \
				echo "    [FAIL] $$name"; \
				fail=$$((fail + 1)); \
			fi; \
		fi; \
	done; \
	echo "    Console: $$success succeeded, $$fail failed"
	@echo ""
	@echo ">>> Building GUI examples (10)..."
	@success=0; fail=0; \
	for dir in $(GUI_EXAMPLES); do \
		name=$$(basename $$dir); \
		lpi=$$(find $$dir -maxdepth 1 -name "*.lpi" | head -1); \
		if [ -n "$$lpi" ]; then \
			if lazbuild "$$lpi" > /dev/null 2>&1; then \
				success=$$((success + 1)); \
			else \
				echo "    [FAIL] $$name"; \
				fail=$$((fail + 1)); \
			fi; \
		fi; \
	done; \
	echo "    GUI: $$success succeeded, $$fail failed"
	@echo ""
	@echo "[OK] Examples build complete!"

# Build everything
build-all: build build-sqlcipher build-examples
	@echo ""
	@echo "==============================================================================="
	@echo "[OK] All builds complete!"
	@echo "==============================================================================="

# Clean and rebuild core
rebuild: clean build

# ==============================================================================
# TEST TARGETS
# ==============================================================================

# Run core tests
test:
	@echo "==============================================================================="
	@echo "Running NDXSQLite core tests..."
	@echo "==============================================================================="
	@echo ""
	@echo ">>> NDXSQLiteTests"
	@echo "-------------------------------------------------------------------------------"
	@cd tests && ./NDXSQLiteTests
	@echo ""
	@echo ">>> NDXSQLiteAdvancedTests"
	@echo "-------------------------------------------------------------------------------"
	@cd tests && ./NDXSQLiteAdvancedTests
	@echo ""
	@echo ">>> DateTimeUtilsTests"
	@echo "-------------------------------------------------------------------------------"
	@cd tests && ./DateTimeUtilsTests --all --format=plain 2>/dev/null || ./DateTimeUtilsTests
	@echo ""
	@echo ">>> DateTimeUtilsFunctionalTests"
	@echo "-------------------------------------------------------------------------------"
	@cd tests && ./DateTimeUtilsFunctionalTests --all --format=plain 2>/dev/null || ./DateTimeUtilsFunctionalTests
	@echo ""
	@echo "==============================================================================="
	@echo "[OK] All core tests completed!"
	@echo "==============================================================================="

# Run SQLCipher tests
test-sqlcipher:
	@echo "==============================================================================="
	@echo "Running SQLCipher tests..."
	@echo "==============================================================================="
	@echo ""
	@echo "Checking SQLCipher availability..."
	@if ldconfig -p 2>/dev/null | grep -q sqlcipher; then \
		echo "SQLCipher found."; \
		echo ""; \
		echo ">>> SQLCipherTests"; \
		echo "-------------------------------------------------------------------------------"; \
		cd tests && ./SQLCipherTests; \
		echo ""; \
		echo "[OK] SQLCipher tests completed!"; \
	else \
		echo "[SKIP] SQLCipher not installed."; \
		echo "       Install with: sudo apt-get install libsqlcipher1 libsqlcipher-dev"; \
	fi
	@echo "==============================================================================="

# Run all tests
test-all: test test-sqlcipher
	@echo ""
	@echo "==============================================================================="
	@echo "[OK] All tests completed!"
	@echo "==============================================================================="

# ==============================================================================
# VERIFICATION
# ==============================================================================

# Full verification (for CI/CD or pre-push)
check: clean-all build build-sqlcipher test test-sqlcipher
	@echo ""
	@echo "==============================================================================="
	@echo "[OK] Full verification passed!"
	@echo "==============================================================================="

# Show project information
info:
	@echo "==============================================================================="
	@echo "NDXSQLite Project Information"
	@echo "==============================================================================="
	@echo ""
	@echo "Version:"
	@grep -E "VERSION_STRING|VERSION_DATE" $(VERSION_FILE) 2>/dev/null | head -2 || echo "  (version file not found)"
	@echo ""
	@echo "Source files:"
	@echo "  Pascal units: $$(find src -name '*.pas' | wc -l)"
	@echo "  Include files: $$(find src -name '*.inc' | wc -l)"
	@echo ""
	@echo "Tests:"
	@echo "  Test programs: $$(find tests -maxdepth 1 -name '*.lpr' | wc -l)"
	@echo "  Test units: $$(find tests -maxdepth 1 -name '*.pas' | wc -l)"
	@echo ""
	@echo "Examples:"
	@echo "  Console: $$(ls -d examples/console/*/ 2>/dev/null | wc -l)"
	@echo "  GUI: $$(ls -d examples/gui/*/ 2>/dev/null | wc -l)"
	@echo ""
	@echo "Documentation:"
	@echo "  Markdown files: $$(find docs -name '*.md' | wc -l)"
	@echo ""
	@echo "Platform: $$(uname -s) ($$(uname -m))"
	@echo "FPC version: $$(fpc -iV 2>/dev/null || echo 'not found')"
	@echo "Lazarus version: $$(lazbuild --version 2>/dev/null | head -1 || echo 'not found')"
	@echo "==============================================================================="

# ==============================================================================
# INSTALL TARGETS
# ==============================================================================

# Build package
package:
	@echo "==============================================================================="
	@echo "Building NDXSQLite package..."
	@echo "==============================================================================="
	@lazbuild --add-package-link=$(PACKAGE_FILE)
	@lazbuild $(PACKAGE_FILE)
	@echo ""
	@echo "[OK] Package built successfully!"

# Install package in Lazarus
install:
	@echo "==============================================================================="
	@echo "Installing NDXSQLite package..."
	@echo "==============================================================================="
	@lazbuild --add-package $(PACKAGE_FILE)
	@echo ""
	@echo "[OK] Package installed!"
	@echo "    Restart Lazarus IDE to use NDXSQLite."

# Uninstall package
uninstall:
	@echo "==============================================================================="
	@echo "Uninstalling NDXSQLite package..."
	@echo "==============================================================================="
	@echo "To uninstall, remove the package from Lazarus IDE:"
	@echo "  1. Open Lazarus"
	@echo "  2. Go to Package > Install/Uninstall Packages"
	@echo "  3. Find NDXSQLite and click 'Uninstall'"
	@echo "  4. Rebuild Lazarus IDE"

# ==============================================================================
# CLEANUP TARGETS
# ==============================================================================

# Clean src/ and tests/
clean: clean-src clean-tests

# Clean src/ directory
clean-src:
	@echo "Cleaning src/..."
	@find src -type d -name "lib" -exec rm -rf {} + 2>/dev/null || true
	@find src -type f \( -name "*.o" -o -name "*.ppu" -o -name "*.compiled" \
		-o -name "*.or" -o -name "*.rst" -o -name "*.rsj" -o -name "link*.res" \
		-o -name "*.bak" -o -name "*~" \) -delete 2>/dev/null || true
	@echo "  [OK] src/ cleaned."

# Clean tests/ directory
clean-tests:
	@echo "Cleaning tests/..."
	@rm -rf tests/lib 2>/dev/null || true
	@find tests -maxdepth 1 -type f \( -name "*.o" -o -name "*.ppu" -o -name "*.compiled" \
		-o -name "*.or" -o -name "*.rst" -o -name "*.rsj" -o -name "link*.res" \
		-o -name "*.bak" -o -name "*~" \) -delete 2>/dev/null || true
	@rm -f tests/NDXSQLiteTests tests/NDXSQLiteNativeTests tests/NDXSQLiteAdvancedTests 2>/dev/null || true
	@rm -f tests/DateTimeUtilsTests tests/DateTimeUtilsFunctionalTests tests/SQLCipherTests 2>/dev/null || true
	@find tests -maxdepth 1 -type f -name "*.db" -delete 2>/dev/null || true
	@find tests -type f \( -name "*.db-wal" -o -name "*.db-shm" -o -name "*.db-journal" \) -delete 2>/dev/null || true
	@rm -rf "tests/..../" "tests/%2e%2e%2f%2e%2e%2fetc/" "tests/C:/" "tests/..%c0%af..%c0%afetc/" 2>/dev/null || true
	@echo "  [OK] tests/ cleaned."

# Clean only lib/ directories everywhere
clean-lib:
	@echo "Cleaning all lib/ directories..."
	@find . -type d -name "lib" -not -path "./.git/*" -exec rm -rf {} + 2>/dev/null || true
	@echo "  [OK] lib/ directories cleaned."

# Clean examples/ directory
clean-examples:
	@echo "Cleaning examples/..."
	@find examples -type d -name "lib" -exec rm -rf {} + 2>/dev/null || true
	@find examples -type f \( -name "*.o" -o -name "*.ppu" -o -name "*.compiled" \
		-o -name "*.or" -o -name "*.rst" -o -name "*.rsj" -o -name "link*.res" \
		-o -name "*.bak" -o -name "*~" -o -name "*.exe" \) -delete 2>/dev/null || true
	@find examples -maxdepth 3 -type f -executable ! -name "*.*" -delete 2>/dev/null || true
	@find examples -type f \( -name "*.db" -o -name "*.db-wal" -o -name "*.db-shm" -o -name "*.db-journal" \) -delete 2>/dev/null || true
	@echo "  [OK] examples/ cleaned."

# Clean everything
clean-all: clean-src clean-tests clean-examples
	@echo ""
	@echo "[OK] All directories cleaned!"

# ==============================================================================
# RELEASE TARGETS
# ==============================================================================

# Create a version tag
# Usage: make tag VERSION=1.0.0
tag:
ifndef VERSION
	$(error VERSION is required. Usage: make tag VERSION=x.y.z)
endif
	@echo "==============================================================================="
	@echo "Creating version $(VERSION)..."
	@echo "==============================================================================="
	@# Parse version components
	$(eval MAJOR := $(word 1,$(subst ., ,$(VERSION))))
	$(eval MINOR := $(word 2,$(subst ., ,$(VERSION))))
	$(eval PATCH := $(word 3,$(subst ., ,$(VERSION))))
	$(eval TODAY := $(shell date +%Y-%m-%d))
	@# Update version file
	@echo "{===============================================================================" > $(VERSION_FILE)
	@echo "  NDXSQLite Version Information" >> $(VERSION_FILE)
	@echo "  (c) 2026 Nicolas DEOUX - NDX Software. MIT License." >> $(VERSION_FILE)
	@echo "" >> $(VERSION_FILE)
	@echo "  This file is automatically updated when creating version tags." >> $(VERSION_FILE)
	@echo "  Do not edit manually - changes will be overwritten." >> $(VERSION_FILE)
	@echo "===============================================================================}" >> $(VERSION_FILE)
	@echo "" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_VERSION_MAJOR = $(MAJOR);" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_VERSION_MINOR = $(MINOR);" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_VERSION_PATCH = $(PATCH);" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_VERSION_STRING = '$(VERSION)';" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_VERSION_DATE = '$(TODAY)';" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_VERSION_FULL = NDXSQLITE_VERSION_STRING;" >> $(VERSION_FILE)
	@echo "  NDXSQLITE_SQLITE_VERSION = '3.45.0';" >> $(VERSION_FILE)
	@echo ""
	@echo "Version file updated: $(VERSION_FILE)"
	@echo ""
	@# Git operations
	@git add $(VERSION_FILE)
	@git commit -m "Release version $(VERSION)"
	@git tag -a v$(VERSION) -m "Version $(VERSION)"
	@echo ""
	@echo "==============================================================================="
	@echo "[OK] Version $(VERSION) created successfully!"
	@echo "==============================================================================="
	@echo ""
	@echo "  - Version file updated"
	@echo "  - Commit created"
	@echo "  - Tag v$(VERSION) created"
	@echo ""
	@echo "To push: git push && git push --tags"
	@echo ""
