FROM fedora:latest

RUN dnf -y install rust cargo llvm12-devel libffi-devel libxml++-devel glibc-static libstdc++-static wget zlib-static ncurses-static findutils

RUN cd /tmp && \
    wget https://github.com/libffi/libffi/releases/download/v3.4.2/libffi-3.4.2.tar.gz && \
    tar xvf libffi-3.4.2.tar.gz && \
    cd libffi-3.4.2 && \
    ./configure --enable-static --disable-shared --disable-docs && \
    make install

RUN cd /tmp && \
    wget http://www.lua.org/ftp/lua-5.4.4.tar.gz && \
    tar xvf lua-5.4.4.tar.gz && \
    cd lua-5.4.4 && \
    make -j all

WORKDIR /home/rust/src

ENV RUSTFLAGS="-C target-feature=+crt-static \
    -L native=/usr/lib64 -L native=/usr/local/lib64 -L native=/usr/lib/gcc/x86_64-redhat-linux/11 \
    -l static=ffi -l static=stdc++ -l static=tinfo"

ENV LUA_LIB="/tmp/lua-5.4.4/src"
ENV LUA_INC="/tmp/lua-5.4.4/src"
ENV LUA_LINK="static"
ENV LUA_LIB_NAME="lua"

CMD mkdir -p target/x86_64-unknown-linux-gnu/release/deps && \
    cp /usr/local/lib64/libffi.a target/x86_64-unknown-linux-gnu/release/deps && \
    cp /usr/lib64/libm.a target/x86_64-unknown-linux-gnu/release/deps && \
    cp /usr/lib64/libmvec.a target/x86_64-unknown-linux-gnu/release/deps && \
    cp /usr/lib64/libz.a target/x86_64-unknown-linux-gnu/release/deps && \
    cp /usr/lib64/libtinfo.a target/x86_64-unknown-linux-gnu/release/deps && \
    cp /tmp/lua-5.4.4/src/liblua.a target/x86_64-unknown-linux-gnu/release/deps && \
    cargo build --release --target x86_64-unknown-linux-gnu && \
    mv target/x86_64-unknown-linux-gnu/release/oktac . && \
    cargo clean