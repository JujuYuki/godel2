FROM ubuntu:bionic
WORKDIR /root
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    ghc cabal-install git ca-certificates ssh-client software-properties-common \
    rename wget cmake make clang-4.0 llvm-4.0-dev ocaml-nox \
    camlp4 libocamlgraph-ocaml-dev camlidl libapron-ocaml-dev \
    libedit-dev zlib1g-dev zlib1g libgmp-dev ocamlbuild && \
    apt-add-repository ppa:mcrl2/release-ppa -y && \
    apt-get update && apt-get install -y --no-install-recommends mcrl2 && \
    cd /usr/bin && rename 's/-4\.0//' *-4.0 && \
    cd /usr/local/src && \
    git clone https://github.com/JujuYuki/godel2.git && \
    cd godel2 && \
    cabal update && cabal install cmdargs parsec ansi-terminal && \
    ghc Godel.hs && mv Godel /usr/bin/ && \
    cd .. && rm -rf godel2 && \
    git clone https://github.com/s-falke/llvm2kittel && \
    git clone https://github.com/s-falke/kittel-koat && \
    mkdir build && cd build && \
    cmake ../llvm2kittel || true && \
    make && mv llvm2kittel /usr/bin && \
    cd ../kittel-koat && \
    HAVE_Z3=false make kittel && mv /usr/local/src/kittel-koat/_build/kittel.native /usr/bin/ && \
    cd .. && rm -rf kittel-koat && rm -rf llvm2kittel && rm -rf build && \
    wget -O yices-1.0.40-x86_64-unknown-linux-gnu-static-gmp.tar.gz \
      'http://yices.csl.sri.com/cgi-bin/yices-newdownload.cgi?file=yices-1.0.40-x86_64-unknown-linux-gnu-static-gmp.tar.gz&accept=I%20accept' && \
    tar zxf yices-1.0.40-x86_64-unknown-linux-gnu-static-gmp.tar.gz && \
    mv yices-1.0.40/include/* /usr/include/ && \
    mv yices-1.0.40/bin/* /usr/bin/ && \
    mv yices-1.0.40/lib/* /usr/lib/ && \
    rm -rf yices-1.0.40* && \
    apt-get remove -y wget camlp4 ocaml-nox cmake git ghc cabal-install --purge &&\
    apt-get autoremove -y --purge&& \
    rm -rf /var/lib/apt/lists/*
