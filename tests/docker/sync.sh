#! /usr/bin/bash

server=""
username=""
password=""
maxage=30

for i in "$@"; do
    case $i in
        --server|-s)
            server=$2
            shift; shift
            ;;
        --username|-u)
            username=$2
            shift; shift
            ;;
        --password|-p)
            password=$2
            shift; shift
            ;;
        --maxage)
            maxage=$2
            shift; shift
            ;;
        *)
            # unknown option
            ;;
    esac
done

if [[ "$server" == "" || "$username" == "" || "$password" == "" ]]; then
    echo "Required: server, username, password"
    exit 1
fi

if [ -z "$(docker ps | grep dovecot)" ]; then
    echo "Dovecot is not running"
    exit 1
fi

docker exec dovecot imapsync \
    --maxage 30 \
    --host1 "$server" --user1 "$username" --password1 "$password" \
    --host2 localhost --user2 vmail --password2 vmail
