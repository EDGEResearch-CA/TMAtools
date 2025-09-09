FROM rocker/tidyverse:4.5.0

RUN R -e 'install.packages(c("languageserver"))'
RUN R -e 'remotes::install_github("nx10/httpgd")'
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    python3-pip \
    pipx
RUN pipx ensurepath
RUN pipx install radian
RUN echo "export PS1='\\[\\e[0;32m\\]\\u@\\h \\[\\e[0;34m\\]\\w \\[\\e[0;31m\\]\\\\$ \\[\\e[0m\\]'" >> ~/.bashrc
RUN mkdir -p /usr/local/bin && ln -s /root/.local/bin/radian /usr/local/bin/radian