# k8s-secret-mode for Emacs

This little minor-mode adds basic encoding and decoding capabilities
to Emacs when you edit Kubernetes Secrets.

Right now, it consists of an Elisp component and a Go component
(because I was too lazy to parse YAML in Elisp). To install the Go
component you can run `go get
github.com/zerok/k8s-secret-mode.el/cmd/...` which will add a
`k8s-secret-codec` binary to your `$PATH`.

Inside your Emacs configuration you can then set the path to this
binary using the `k8s-secret-codec-bin` custoization variable.
