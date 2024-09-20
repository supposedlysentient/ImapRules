FROM dovecot/dovecot

ARG dependencies="libauthen-ntlm-perl libcgi-pm-perl libcrypt-openssl-rsa-perl libdata-uniqid-perl libencode-imaputf7-perl libfile-copy-recursive-perl libfile-tail-perl libio-socket-inet6-perl libio-socket-ssl-perl libio-tee-perl libhtml-parser-perl libjson-webtoken-perl libmail-imapclient-perl libparse-recdescent-perl libproc-processtable-perl libmodule-scandeps-perl libreadonly-perl libregexp-common-perl libsys-meminfo-perl libterm-readkey-perl libtest-mockobject-perl libtest-pod-perl libunicode-string-perl liburi-perl libwww-perl libtest-nowarnings-perl libtest-deep-perl libtest-warn-perl libnet-server-perl make time cpanminus"
ARG imapsync_uri="https://raw.githubusercontent.com/imapsync/imapsync/master/imapsync"
ARG vmail_password="vmail"

RUN ln -s /bin/bash /usr/bin/bash

RUN apt update && \
    apt install -y $dependencies

RUN apt install -y wget procps && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN wget -N $imapsync_uri -O /usr/local/bin/imapsync && \
    chmod +x /usr/local/bin/imapsync

RUN usermod -p $(perl -e 'print crypt($ARGV[0], "password")' "${vmail_password}") vmail
