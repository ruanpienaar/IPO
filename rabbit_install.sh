#!/bin/bash

function startup(){
  echo "More help at https://www.rabbitmq.com/install-generic-unix.html#"
  echo "Rabbit nodes, normally are installed on seperate hosts"
  echo "On each rabbit machine: export RABBITMQ_NODENAME=\"proc_buff@<HOSTNAME>\" and export RABBITMQ_NODENAME=\"out_buff@<HOSTNAME>\"."
  echo "Configure rabbit in \$RABBITMQ_HOME/etc/rabbitmq/rabbitmq-env.conf"
  echo "To Start rabbit run (proc_buff/out_buff) rabbitmq_server-3.5.3/sbin/rabbitmq-server -detached"
  echo "Plugins: rabbitmq_server-3.5.3/sbin/rabbitmq-plugins list"
  echo "Start management with: rabbitmq_server-3.5.3/sbin/rabbitmq-plugins enable rabbitmq_management"
}

if [ -d "rel/proc_buff/rabbitmq_server-3.5.3" && -d "rel/out_buff/rabbitmq_server-3.5.3" ]; then
        startup
else
	wget -c https://www.rabbitmq.com/releases/rabbitmq-server/v3.5.3/rabbitmq-server-generic-unix-3.5.3.tar.gz
        tar -xvzf rabbitmq-server-generic-unix-3.5.3.tar.gz
        cp -r rabbitmq_server-3.5.3 rel/proc_buff/
        cp -r rabbitmq_server-3.5.3 rel/out_buff/
        
        cp rabbitmq.proc_buff.config rel/proc_buff/rabbitmq_server-3.5.3/etc/rabbitmq/rabbitmq.config
        cp rabbitmq.out_buff.config rel/out_buff/rabbitmq_server-3.5.3/etc/rabbitmq/rabbitmq.config
        
        cp enabled_plugins rel/proc_buff/rabbitmq_server-3.5.3/etc/rabbitmq/
        cp enabled_plugins rel/out_buff/rabbitmq_server-3.5.3/etc/rabbitmq/
	
	tar -cvf rel/proc_buff.tar rel/proc_buff
	tar -cvf rel/out_buff.tar rel/out_buff
        
        rm rabbitmq-server-generic-unix-3.5.3.tar.gz
        rm -rf rabbitmq_server-3.5.3
        startup
fi
