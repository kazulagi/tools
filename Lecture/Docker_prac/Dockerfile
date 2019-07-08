
FROM centos:centos7

RUN yum -y update

RUN yum -y install gcc

ADD hello.c .

RUN gcc hello.c
ENTRYPOINT ["/bin/bash"]
CMD ["-c","./a.out"]
