# Public test-only TLS material

This self-signed localhost certificate and its private key are public test
fixtures, generated exclusively to test certificate verification. Never use
them for production identity or trust. The certificate has only DNS:localhost
in its subject alternative names so that connections to 127.0.0.1 must fail
hostname verification even when its issuing certificate is explicitly trusted.
