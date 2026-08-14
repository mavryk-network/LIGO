FROM node:18-bullseye

# MAVRYK: harden the image build against the flaky deb.debian.org mirror + transient network resets
# ("Connection reset by peer" from the CI runner). Make apt retry each download (it re-resolves the CDN,
# so a bad edge node is skipped) and make wget retry too, so a transient package/asset fetch doesn't fail
# the whole build. Pairs with the GitLab job-level `retry:` on the vscode-extension-* jobs.
RUN printf 'Acquire::Retries "8";\nAcquire::http::Timeout "60";\nAcquire::https::Timeout "60";\n' > /etc/apt/apt.conf.d/80-mavryk-retries \
 && printf 'tries = 5\nretry_connrefused = on\nwaitretry = 5\ntimeout = 60\n' > /etc/wgetrc

# Install vscode runtime dependencies that will later be used by vscode downloaded separately.
# Extest doesn't work with vscode installed this way for some reason >:(

ARG VSCODE_VERSION="1.81.1"
RUN wget -q https://update.code.visualstudio.com/${VSCODE_VERSION}/linux-deb-x64/stable -O code.deb
RUN apt-get -y update && apt install libffi-dev && apt-get -y install ./code.deb && apt-get -y remove code

# To avoid downloading vscode and chrome driver every time
RUN mkdir -p /vscode/.vscode-test/ /vscode/test-resources/VSCode-linux-x64
# Needed for extest-based tests
RUN wget -q https://chromedriver.storage.googleapis.com/108.0.5359.71/chromedriver_linux64.zip
RUN unzip chromedriver_linux64.zip -d /vscode/test-resources/
RUN wget -q https://update.code.visualstudio.com/${VSCODE_VERSION}/linux-x64/stable -O stable.tar.gz
RUN tar -xf stable.tar.gz -C /vscode/test-resources/
RUN ln -s /vscode/test-resources/VSCode-linux-x64 /vscode/.vscode-test/vscode-linux-x64-${VSCODE_VERSION}
# Needed for running tests in headless mode
RUN apt-get -y install xvfb xclip


ARG MAVKIT_VERSION="v20.3"
# MAVRYK: mavryk-packaging tags are now `vX.Y` (no "mavkit-" prefix, no "-1" suffix); v20.3 is the latest that ships a mavkit-client asset.
RUN wget -q https://github.com/mavryk-network/mavryk-packaging/releases/download/${MAVKIT_VERSION}/mavkit-client -O /usr/bin/mavkit-client
RUN chmod +x /usr/bin/mavkit-client

# Some runtime dependency for ligo
RUN yarn global add esy

COPY ligo /usr/bin/
RUN chmod +x /usr/bin/ligo

ENV USER vscode
RUN useradd --create-home vscode
COPY ./tools/vscode /vscode

# Broken symlink
RUN rm /vscode/LICENSE.md && touch /vscode/LICENSE.md

RUN mkdir /vscode/bin
COPY ./Linux/bin/ligo-debugger /vscode/bin
RUN chmod +x /vscode/bin/ligo-debugger

# To avoid running vscode as a root
RUN chown -R vscode:vscode /vscode
WORKDIR /vscode
