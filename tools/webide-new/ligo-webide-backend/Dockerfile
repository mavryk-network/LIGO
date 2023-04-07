FROM tezos/tezos:latest as octez-client
FROM ligolang/ligo:0.63.2 as ligo-compiler
FROM registry.gitlab.com/ligolang/ligo/stack-haskell:latest as web-ide-builder
LABEL stage=webide-new-builder
#
# Build ide backend
#

COPY . .


RUN stack build --copy-bins --local-bin-path /webide_bin/

FROM ubuntu:22.04

RUN mkdir /webide_bin/

COPY --from=octez-client /usr/lib/ /usr/lib/
COPY --from=octez-client /lib/ /lib/
COPY --from=octez-client /usr/local/bin/octez-client /usr/local/bin/octez-client
COPY --from=octez-client /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=web-ide-builder /lib/ /lib/
COPY --from=web-ide-builder /webide_bin /webide_bin/
COPY --from=ligo-compiler /root/ligo /usr/local/bin/ligo

ENTRYPOINT [ "/webide_bin/ligo-webide-backend", "-p", "8080", "--ligo-path", "/usr/local/bin/ligo", "--octez-client-path", "/usr/local/bin/octez-client", "--verbose" ]
