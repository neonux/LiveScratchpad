# To build an xpi of current HEAD in build/:
# $ make
#
# Env var OBJDIR is required for 'test' and 'testcoverage' targets.
# Env var MOZILLA_SRC is required for 'testcoverage' target.
#
# On Linux you probably want to 'test' or 'testcoverage' with :
# $ nice xvfb-run make test
# So that the desktop is not monopolized by the tests.

PROJECT="LiveScratchpad"
PWD=`pwd`
VERSION=`git describe --tags --dirty | tail -c +2`
XPI="${PWD}/build/${PROJECT}-${VERSION}.xpi"
COVERAGE_DIR="${PWD}/build/coverage/${VERSION}"

export TEST_PATH=browser/base/content/test/${PROJECT}

export COVERAGE_PROJECT=${PROJECT}
export COVERAGE_FILTER=*/LiveEvaluator.jsm */LiveEvaluatorUI.jsm
COVERAGE_DIR="${PWD}/build/coverage/${VERSION}"

.PHONY: xpi test testcoverage

xpi:
	@echo "Building '${XPI}'..."
	@mkdir -p build
	@git archive --format=zip -o ${XPI} HEAD

test:
	@ln -sfT `pwd`/test/ui ${OBJDIR}/_tests/testing/mochitest/browser/${TEST_PATH}
	@make -C ${OBJDIR} mochitest-browser-chrome

testcoverage:
	@ln -sfT `pwd`/test/ui ${OBJDIR}/_tests/testing/mochitest/browser/${TEST_PATH}
	@mkdir -p ${COVERAGE_DIR}
	-@COVERAGE=1 COVERAGE_OUTPUT="${COVERAGE_DIR}/coverage_raw.json" make -C ${OBJDIR} mochitest-browser-chrome
	@python ${MOZILLA_SRC}/testing/tools/coverage/aggregate.py ${COVERAGE_DIR}/coverage_raw.json > ${COVERAGE_DIR}/coverage.json
	@cd ${COVERAGE_DIR}; COVERAGE_VERSION=${VERSION} python ${MOZILLA_SRC}/testing/tools/coverage/report.py coverage.json
	@echo "Test coverage report ready: ${COVERAGE_DIR}/index.html"
