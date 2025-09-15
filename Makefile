
.PHONY= update build optim todo serve clean

all: clean update build optim

update:
	wasm32-wasi-cabal update

todo:
	find src bench spec -name "*.hs" | xargs grep -i todo

build:
	wasm32-wasi-cabal build app
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public result

