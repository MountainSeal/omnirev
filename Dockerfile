FROM fpco/stack-build:lts-12.20 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:bionic
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt update && apt install -y \
  ca-certificates \
  libgmp-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.20/8.4.4/bin .
#CMD [ "/opt/app/omnirev" ]