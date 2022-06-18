FROM fpco/stack-build:lts-18.28 as build
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack install --system-ghc

FROM ubuntu:focal as prod
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN  apt update && apt upgrade -y
COPY ./server/static /opt/app/static
COPY --from=build /root/.local/bin .
# ENTRYPOINT [ "/opt/app/server" ]