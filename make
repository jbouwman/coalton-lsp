#!/usr/bin/env nu

def build [] {
    (sbcl --noinform
          --non-interactive
          --eval "(asdf:load-system :coalton-lsp)"
          --eval "(sb-ext:save-lisp-and-die \"coalton-lsp\" :toplevel #'coalton-lsp:main :executable t)")
}

def test [] {
    (sbcl --noinform
          --non-interactive
          --eval "(asdf:load-system :coalton-lsp/tests)"
	  --eval "(fiasco:run-package-tests
                    :packages '(:coalton-lsp/tests)
                    :interactive nil)")
}

def main [args] {
    match $args {
          "build" => build,
          "test" => test
    }
}
