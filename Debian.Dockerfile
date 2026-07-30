FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y \
      curl \
      wget \
      git \
      sed \
      rsync \
      m4 \
      build-essential \
      patch \
      unzip \
      opam \
      jq \
      bc \
    && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://get.docker.com/ | sh
