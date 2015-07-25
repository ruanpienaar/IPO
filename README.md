# IPO
Input Processing Output

*OTP 18 >= compliant...

based on the [IPO](http://en.wikipedia.org/wiki/IPO_Model) pattern. 
With the added benefit of having a buffer layer between in, process and output.
The buffers will be utilizing [amqp](https://www.amqp.org/) through [rabbitmq](http://www.rabbitmq.com).
The In and Out nodes will act as the [transport layers](http://en.wikipedia.org/wiki/Transport_layer).

There are 5 nodes:

in -> proc_buff -> proc -> out_buff -> out

## In

This node will be accepting incoming data, on preconfigured protocols.
Currently tcp ip version 4 has been implemented. Http will follow soon.
Here is a example sys.config file ( in /rel/in/files/sys.config ) :

```Erlang
...
 {in,[
    {incoming_protocols, [
        {protocol,[
            {type, tcp_v4_socket},
            {tcp_v4_port, 8888},
            {listen_opts,
                [binary,
                 {packet, raw},
                 {active, false},
                 {reuseaddr, true},
                 {ip,{0,0,0,0}}]
            }
        ]}
        ,
        {protocol,[
            {type, http},
            {http_port, 9999}
        ]}
    ]}
 ]},
...
```

## Process Buffer

> **In-Progress**

This node will be a rabbitmq-server node

## Processing

> **In-Progress**

This node will be dealing with the processing of the incoming data, whether it be 
through a callback module, external service, or just pass through.

## Output Buffer

> **In-Progress**

This node will be a rabbitmq-server node

## Output

> **In-Progress**

This node will be simmilar to the In Node,
responding with the processed data.

### Install

> **In-Progress**

_still have to see how the rabbit server nodes will be installed in a fairly easy way_

1. clone
2. cd 
3. make
4. Run the rebar shell scripts created in each node's release directory.
