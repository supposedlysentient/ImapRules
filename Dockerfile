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

# TODO: validate this matches JsonConfig
ENV IMAPRULES_SERVER="" \
    IMAPRULES_PORT="" \
    IMAPRULES_SSL="" \
    IMAPRULES_USERNAME="" \
    IMAPRULES_PASSWORD="" \
    IMAPRULES_CREDENTIAL_PATH="" \
    IMAPRULES_RULE_PATH="" \
    IMAPRULES_CHECKPOINT_PATH="" \
    IMAPRULES_LOG_PATH="" \
    IMAPRULES_LOG_CONSOLE="" \
    IMAPRULES_VERBOSITY=""

COPY --from=build /ImapRules/Release/ /usr/bin/ImapRules/

RUN ln -s /usr/bin/ImapRules/ImapRules /usr/bin/imaprules

ENV IMAPRULES_CONFIG="/etc/imaprules/config.json"
RUN mkdir /etc/imaprules && \
    mv /usr/bin/ImapRules/config.json ${IMAPRULES_CONFIG}

WORKDIR /etc/imaprules
VOLUME /etc/imaprules

CMD [ "imaprules", "--daemonize" ]
