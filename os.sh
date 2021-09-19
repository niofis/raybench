cat /etc/os-release | ack '^PRETTY_NAME="(.+)"$' --output='$1'
