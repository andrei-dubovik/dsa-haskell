.PHONY: all clean

haskell := dsa-haskell/dsa-haskell
rust_declarative := dsa-rust-declarative/target/release/dsa-declarative
rust_imperative := dsa-rust-imperative/target/release/dsa-imperative

all: $(haskell) $(rust_declarative) $(rust_imperative)

bench: all
	./$(haskell)
	./$(rust_declarative)
	./$(rust_imperative)

$(haskell): dsa-haskell/dsa-haskell.hs
	cd dsa-haskell; ghc -dynamic -O2 -no-keep-hi-files -no-keep-o-files dsa-haskell.hs

$(rust_declarative): $(addprefix dsa-rust-declarative/,Cargo.toml src/main.rs src/window.rs)
	cd dsa-rust-declarative; cargo build --release

$(rust_imperative): $(addprefix dsa-rust-imperative/,Cargo.toml src/main.rs)
	cd dsa-rust-imperative; cargo build --release

clean:
	# *.lock files are kept
	rm -rf dsa-haskell/dsa-haskell dsa-rust-{declarative,imperative}/target
