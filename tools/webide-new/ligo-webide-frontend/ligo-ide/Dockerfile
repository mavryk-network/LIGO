# To build this image locally, first copy the syntaxes directory:
# cp -r ../../../vscode/syntaxes ./syntaxes-temp
# Then run: docker build .
# For newer Docker with buildx, you can use:
# docker buildx build --build-context syntaxes=../../../vscode/syntaxes .

ARG BUILDER_IMAGE="node:18"
ARG RUNNER_IMAGE="joseluisq/static-web-server:2.9-alpine"

FROM ${BUILDER_IMAGE} as builder

WORKDIR /app

ENV NODE_OPTIONS=--max_old_space_size=4096

COPY . .

RUN yarn install

ENV MEASUREMENT_ID="G-5H01XTN4PY"
ENV GIT_PROXY="https://ligo-webide-cors-proxy.serokell.team"
ARG backend_url
ARG http_protocol
ENV HTTP_PROTOCOL=$http_protocol
ENV BACKEND_URL=$backend_url
ENV NODE_OPTIONS=--openssl-legacy-provider
# Copy syntaxes from temp directory (copied by CI before build)
COPY syntaxes-temp ./src/ligo-components/ligo-project/Project/languages/syntaxes/
RUN echo $(ls ./src/ligo-components/ligo-project/Project/languages/syntaxes/)
RUN yarn build:react-prod

FROM ${RUNNER_IMAGE}

LABEL maintainer="infra@trili.tech"
ENV NODE_ENV production
ENV SERVER_FALLBACK_PAGE /public/index.html
COPY --from=builder /app/build /public

EXPOSE 3000

ENTRYPOINT ["static-web-server", "--root", "/public" , "-g", "INFO" ,"-p" , "3000"]
