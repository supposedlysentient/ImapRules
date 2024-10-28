FROM mcr.microsoft.com/dotnet/sdk:8.0-bookworm-slim AS build

WORKDIR /ImapRules/

COPY ./.config/ /ImapRules/.config/
RUN dotnet tool restore

COPY global.json paket.* /ImapRules/
RUN dotnet paket restore

COPY global.json Build* config.schema.json /ImapRules/
COPY ./src/ /ImapRules/src/

RUN dotnet run buildRelease

####################################

FROM mcr.microsoft.com/dotnet/runtime:8.0-bookworm-slim

COPY --from=build /ImapRules/Release/ /usr/bin/ImapRules/

RUN ln -s /usr/bin/ImapRules/ImapRules /usr/bin/imaprules

RUN mkdir /etc/imaprules && \
    mv /usr/bin/ImapRules/config.json /etc/imaprules/

WORKDIR /etc/imaprules
VOLUME /etc/imaprules

ARG VERSION=""
LABEL org.opencontainers.image.authors="imaprules@sckr.link" \
      org.opencontainers.image.description="IMAP agent that connects to your mailbox and runs mail rules." \
      org.opencontainers.image.documentation="https://github.com/fsackur/ImapRules/README.md" \
      org.opencontainers.image.licenses="https://github.com/fsackur/ImapRules/LICENSE" \
      org.opencontainers.image.source="https://github.com/fsackur/ImapRules/" \
      org.opencontainers.image.title="ImapRules" \
      org.opencontainers.image.url="https://github.com/fsackur/ImapRules/" \
      org.opencontainers.image.vendor="Freddie Sackur" \
      org.opencontainers.image.version="${VERSION}"

CMD [ "imaprules", "--daemonize" ]
