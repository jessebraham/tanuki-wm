all: tanuki-wm

tanuki-wm:
	mkdir -p ./bin
	raco exe -o ./bin/tanuki-wm main.rkt

.PHONY: clean
clean:
	rm -rf ./bin

