FROM base/archlinux
MAINTAINER kinoru <xkinoru@gmail.com>

RUN cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup
RUN sed -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.backup
RUN rankmirrors -n 6 /etc/pacman.d/mirrorlist.backup > /etc/pacman.d/mirrorlist
RUN sed -i 's/^#SigLevel = .*$/SigLevel = Never/g' /etc/pacman.conf
RUN pacman -Sy --noconfirm
RUN pacman -S archlinux-keyring --noconfirm
RUN pacman -S git fish --noconfirm

# create user
RUN useradd -m user
RUN chsh -s /usr/bin/fish user
USER user
WORKDIR /home/user
