FROM centos:centos7.1.1503
ENV container docker
RUN export http_proxy=http://<Proxy-server>:8080 && \
    export https_proxy=http://<Proxy-server>:8080 && \
    yum swap -y fakesystemd systemd && \
    yum update -y && \
    yum install -y epel-release && \
    yum install -y nginx && \
    yum clean all && \
    ln -sf  /usr/share/zoneinfo/Asia/Tokyo /etc/localtime
ADD nginx.conf /etc/nginx/
ADD default.conf /etc/nginx/conf.d/
RUN systemctl enable nginx
EXPOSE 80
