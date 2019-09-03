FROM ubuntu:bionic
ARG local_bin_path
RUN mkdir -p /root/.local/bin && mkdir -p /root/myapp
ENV PATH /root/.local/bin:$PATH
WORKDIR /root/myapp
COPY ${local_bin_path} /root/.local/bin