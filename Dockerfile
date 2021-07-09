FROM archlinux:latest
RUN pacman -Syyu --noconfirm && \
    pacman -S  base-devel --noconfirm && \
    pacman -S xorg xorg-xauth --noconfirm && \
    pacman -S emacs --noconfirm

RUN xauth add 'Hephaestus/unix:0  MIT-MAGIC-COOKIE-1  e54622abd3e6763b43ed9b40b000dc2e'

CMD ["/usr/bin/emacs"]
