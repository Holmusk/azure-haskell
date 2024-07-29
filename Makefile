SRC=$(shell find azure-auth/ azure-key-vault/ azure-blob-storage/ -type f -name '*.hs')

.PHONY: format
format: $(SRC)
    # we use fourmolu v16
	fourmolu --mode inplace $^
