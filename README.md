# coalton-lsp

A Microsoft Language Server Protocol implementation for Coalton

# hacking

1. Install quicklisp and link this system into local projects

```
% cd ~/quicklisp/local-projects/
% ln -s ../../git/coalton-lsp .
% ls -l
lrwxr-xr-x  1 wolfman  creatures  21 Oct 31 00:00 coalton-lsp -> ../../git/coalton-lsp
```

2. Register the system and load it

```
; SLIME 2.30.git
CL-USER> (ql:register-local-projects)
NIL
CL-USER> (ql:quickload "coalton-lsp")
To load "coalton-lsp":
  Load 1 ASDF system:
    coalton-lsp
; Loading "coalton-lsp"
...
;; COALTON starting in development mode
;; COALTON starting with specializations enabled
;; COALTON starting with heuristic inlining disabled
;; COALTON will emit type annotations
....................
("coalton-lsp")
CL-USER> 
```

3. Start a network server

```
CL-USER> (coalton-lsp:start-network-server)
;; 09:24:46 | info | #<NETWORK-SERVER json-rpc://127.0.0.1:7887 {701D125623}> started
; No value
CL-USER> 
```

4. Connect, using an LSP client

### Visual Studio Code

Compile and run https://github.com/jbouwman/vscode-coalton

### Eglot

docme

### LSP-mode

docme

# build and test

Install nushell and

```
./make test
```
