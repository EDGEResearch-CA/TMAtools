FROM rocker/tidyverse:4.5.0

RUN R -e 'install.packages(c("languageserver"))'
RUN R -e 'remotes::install_github("nx10/httpgd")'
# RUN apt-get update && \
#     apt-get install -y --no-install-recommends \
#     python3.12-venv \
#     python3-pip
# RUN python3 -m venv /workspaces/TMAtools/.venv/TMAtools
# RUN ln -S 
# RUN pip3 install radian