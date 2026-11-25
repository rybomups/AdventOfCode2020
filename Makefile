# === Standard Params ===
CC = ghc
CC_FLAGS = -outputdir $(BUILD_DIR) --make -Wall

# === Files ===
BASE_EXT = hs
BASE_DIR = days\Day$(DAY)
BUILD_DIR = build
FILE_TEMPLATE = FileTemplate.$(BASE_EXT)
SOURCE_FILE = $(BASE_DIR)\day$(DAY)_p$(PART)
SOURCE_FILE_P1 = $(BASE_DIR)\day$(DAY)_p1
INPUT_FILE = $(BASE_DIR)\day$(DAY).txt
INPUT_FILE_WARMUP = $(BASE_DIR)\day$(DAY)_warmup.txt

ENV_FILE = .env
FETCH_INPUT_SCRIPT = fetchInput.js

# === Parameter Requirements ===
# Require DAY and PART to be defined if not cleaning
ifneq (clean, $(MAKECMDGOALS))
  ifndef $(and DAY, PART)
    $(error DAY and PART must be defined)
  endif
endif

# === Targets ===
all: $(SOURCE_FILE)

%: %.$(BASE_EXT)
	$(CC) $(CC_FLAGS) $^ -o $@

.PHONY: run warmup new clean
run: $(SOURCE_FILE)
	@echo === Running ===
	@$^ $(INPUT_FILE)
	@echo === Done ===

warmup: $(SOURCE_FILE)
	@echo === Running ===
	@$^ $(INPUT_FILE_WARMUP)
	@echo === Done ===

new:
ifeq ($(PART), 1)
	-mkdir $(BASE_DIR)
	-if not exist $(SOURCE_FILE).$(BASE_EXT) copy $(FILE_TEMPLATE) $(SOURCE_FILE).$(BASE_EXT)
	-if not exist $(INPUT_FILE)              node --env-file=$(ENV_FILE) $(FETCH_INPUT_SCRIPT) $(DAY) > $(INPUT_FILE)
	-if not exist $(INPUT_FILE_WARMUP)       copy nul $(INPUT_FILE_WARMUP)
else
	-if not exist $(SOURCE_FILE).$(BASE_EXT) copy $(SOURCE_FILE_P1).$(BASE_EXT) $(SOURCE_FILE).$(BASE_EXT)
endif

clean:
	-del /s /f /q *.exe $(BUILD_DIR)\*
