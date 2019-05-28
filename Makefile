all: bin/k8s-secret-codec
bin:
	mkdir -p bin
bin/k8s-secret-codec: $(shell find . -name '*.go') go.mod go.sum bin
	cd cmd/k8s-secret-codec && go build -o ../../bin/k8s-secret-codec
clean:
	rm -rf bin
