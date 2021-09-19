cat /proc/cpuinfo | ack '^model name.+: (.+)$' --output='$1' | head -1
