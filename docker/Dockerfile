FROM mopsa/mopsa-build:latest

USER root
RUN apt-get update && apt-get install -y nano
USER mopsa

ENV PATH="/home/mopsa/bin:${PATH}"

WORKDIR /home/mopsa


RUN \
    eval `opam config env` && \
    opam env >> .profile && \
    echo ". /home/mopsa/banner.sh" >> .profile && \
    opam env >> .bashrc && \
    echo ". /home/mopsa/banner.sh" >> .bashrc && \
    git clone --progress https://gitlab.com/mopsa/mopsa-analyzer.git && \
    git git checkout fix/py_module_setattr && \
    cd mopsa-analyzer && \
    ./configure --prefix /home/mopsa && \
    make -j && \
    make install && \
    make tests && \
    mopsa -v && \
    cd /home/mopsa && \
    mkdir workspace

COPY banner.sh /home/mopsa/

# avoid having to run `eval $(opam env)` at each `docker run`
#ENTRYPOINT ["opam", "exec", "--"]
#CMD ["/bin/bash", "--login"]
