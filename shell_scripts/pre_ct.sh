#!/usr/bin/env bash

echo "%% ======================= CT: Pre Setup Phase [START] ======================= %%"
start_time=$(date +%s)
echo "STEP : Starting containers ........."
services=(
  "pact_ct_postgres"
  "pact_ct_broker_app"
)
# Detect "docker compose" vs "docker-compose", prefer "docker compose" if available
if docker compose >/dev/null 2>&1; then
  COMPOSE_BIN="docker compose"
elif docker-compose >/dev/null 2>&1; then
  COMPOSE_BIN="docker-compose"
else
  echo "ERROR: docker compose or docker-compose not found"
  exit 1
fi
$COMPOSE_BIN -f ct-docker-compose.yml up -d "${services[@]}"
echo "STEP : Wait for services to be accessible .................................."
chmod +x ./shell_scripts/health_check.sh
./shell_scripts/health_check.sh "${services[@]}"
if [ $? -ne 0 ]; then
    exit 1
fi
end_time=$(date +%s)
elapsed_time=$((end_time - start_time))
echo "Execution time: $elapsed_time seconds"
echo "%% ======================= CT: Pre Setup Phase [END] ========================= %%"

