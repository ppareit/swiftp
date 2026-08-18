#!/usr/bin/env bash

set -u

readonly EMULATOR_IP="10.0.2.16"
readonly FTP_PORT="2121"
readonly ADB_FTP_PORT="12121"
readonly PASSIVE_PORT_LOW="50000"
readonly PASSIVE_PORT_HIGH="50009"
readonly ADB_PASSIVE_PORT_LOW="15000"
readonly SERVICE_NAME="Android SwiFTP Server"

publisher_pid=""
ftp_proxy_pid=""
passive_proxy_pids=()
added_address=false

ensure_adb_forward() {
    local host_port="$1"
    local device_port="$2"
    local forwards="$3"

    if grep -Eq -- "[[:space:]]tcp:${host_port}[[:space:]]+tcp:${device_port}$" <<< "$forwards"; then
        return
    fi

    if adb forward "tcp:$host_port" "tcp:$device_port"; then
        echo "Restored ADB forward: tcp:$host_port -> tcp:$device_port"
    fi
}

ensure_adb_forwards() {
    local adb_port
    local forwards
    local passive_port
    forwards="$(adb forward --list 2>/dev/null)" || return

    ensure_adb_forward "$ADB_FTP_PORT" "$FTP_PORT" "$forwards"
    for ((passive_port = PASSIVE_PORT_LOW; passive_port <= PASSIVE_PORT_HIGH; passive_port++)); do
        adb_port=$((ADB_PASSIVE_PORT_LOW + passive_port - PASSIVE_PORT_LOW))
        ensure_adb_forward "$adb_port" "$passive_port" "$forwards"
    done
}

cleanup() {
    echo
    echo "Stopping SwiFTP emulator bridge..."

    for process_id in \
        "$publisher_pid" \
        "$ftp_proxy_pid" \
        "${passive_proxy_pids[@]}"
    do
        if [[ -n "$process_id" ]]; then
            kill "$process_id" 2>/dev/null || true
            wait "$process_id" 2>/dev/null || true
        fi
    done

    adb forward --remove "tcp:$ADB_FTP_PORT" 2>/dev/null || true
    for ((passive_port = PASSIVE_PORT_LOW; passive_port <= PASSIVE_PORT_HIGH; passive_port++)); do
        adb_port=$((ADB_PASSIVE_PORT_LOW + passive_port - PASSIVE_PORT_LOW))
        adb forward --remove "tcp:$adb_port" 2>/dev/null || true
    done

    if [[ "$added_address" == true ]]; then
        sudo ip address delete "$EMULATOR_IP/32" dev lo 2>/dev/null || true
    fi

    echo "Bridge stopped."
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

for required_command in adb avahi-publish-service ip socat sudo; do
    if ! command -v "$required_command" >/dev/null; then
        echo "Missing required command: $required_command" >&2
        exit 1
    fi
done

if ! adb get-state >/dev/null 2>&1; then
    echo "No active ADB device or emulator found." >&2
    exit 1
fi

sudo -v

if ! ip -4 address show dev lo | grep -q -- "$EMULATOR_IP/32"; then
    sudo ip address add "$EMULATOR_IP/32" dev lo
    added_address=true
fi

ensure_adb_forwards
socat \
    "TCP-LISTEN:$FTP_PORT,reuseaddr,fork" \
    "TCP:127.0.0.1:$ADB_FTP_PORT" &
ftp_proxy_pid=$!

for ((passive_port = PASSIVE_PORT_LOW; passive_port <= PASSIVE_PORT_HIGH; passive_port++)); do
    adb_port=$((ADB_PASSIVE_PORT_LOW + passive_port - PASSIVE_PORT_LOW))
    socat \
        "TCP-LISTEN:$passive_port,reuseaddr,fork" \
        "TCP:127.0.0.1:$adb_port" &
    passive_proxy_pids+=("$!")
done

echo "SwiFTP emulator bridge is ready."
echo
echo "Configure SwiFTP:"
echo "  FTP port:          $FTP_PORT"
echo "  Passive port low:  $PASSIVE_PORT_LOW"
echo "  Passive port high: $PASSIVE_PORT_HIGH"
echo
echo "Connect to: ftp://$EMULATOR_IP:$FTP_PORT/"
echo "Waiting for SwiFTP to start..."

while true; do
    ensure_adb_forwards

    if adb shell ss -H -ltn 2>/dev/null |
       grep -Eq -- "[:.]${FTP_PORT}[[:space:]]"
    then
        if [[ -z "$publisher_pid" ]] ||
           ! kill -0 "$publisher_pid" 2>/dev/null
        then
            avahi-publish-service \
                "$SERVICE_NAME" \
                _ftp._tcp \
                "$FTP_PORT" &
            publisher_pid=$!

            echo "SwiFTP detected: NSD service published."
        fi
    elif [[ -n "$publisher_pid" ]]; then
        kill "$publisher_pid" 2>/dev/null || true
        wait "$publisher_pid" 2>/dev/null || true
        publisher_pid=""

        echo "SwiFTP stopped: NSD service withdrawn."
    fi

    sleep 1
done
