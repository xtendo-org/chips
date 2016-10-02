FROM centos:7
MAINTAINER XT <e@xtendo.org>

RUN yum install -y git
RUN curl -Lo /etc/yum.repos.d/shells:fish:release:2.repo http://download.opensuse.org/repositories/shells:fish:release:2/CentOS_7/shells:fish:release:2.repo
RUN yum install -y fish

# create user
RUN useradd -m user
RUN chsh -s /usr/bin/fish user
USER user
WORKDIR /home/user
