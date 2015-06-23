#!/bin/bash
while true; do
    read -p "Do you wish to clean rabbit ( rm -rf ) [y|n]" yn
    case $yn in
        [Yy]* ) rm -rf rel/proc_buff/rabbitmq_server-3.5.3/; rm -rf rel/out_buff/rabbitmq_server-3.5.3/; break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done
