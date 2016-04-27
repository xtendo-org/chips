FROM base/archlinux
MAINTAINER kinoru <xkinoru@gmail.com>

RUN echo "Server = http://mirror.premi.st/archlinux/\$repo/os/\$arch" > /etc/pacman.d/mirrorlist
RUN sed -i 's/^#SigLevel = .*$/SigLevel = Never/g' /etc/pacman.conf
RUN pacman -Sy --noconfirm
RUN pacman -S archlinux-keyring --noconfirm
RUN pacman -S git fish --noconfirm

# create user
RUN useradd -m user
RUN chsh -s /usr/bin/fish user
USER user
WORKDIR /home/user
