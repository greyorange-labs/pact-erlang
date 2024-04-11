#!/usr/bin/env bash

chmod +x ./shell_scripts/wait-for-it.sh
services=("$@")


for service in "${services[@]}"; do
    health_check_port=$(docker exec "$service" sh -c 'echo $HEALTH_CHECK_PORT')
    ./shell_scripts/wait-for-it.sh -h localhost -p $health_check_port -t 60
    if [ $? -ne 0 ]; then
        echo "Error:  health check failed for $service"
        exit 1
    fi
    echo "Service $service is now accessible."
done

echo "All required services are accessible."
