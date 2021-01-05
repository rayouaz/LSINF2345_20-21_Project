# General

- there is a brief explanation about the results without further details about how nodes behave during the different phases, it is hard to asses your understanding of the PS service behaviour

# Sources
- no comments in erl sources to understand how did you implement the PS service
- there is an script that deploys the P2P network and parses the logs
- the implementation of the service is presented in a separate file where one can see the message-passing model with no documentation
- the method `project:launch()` starts the scenario but there is timers that indicating the transition of the different phases
- your submission, as it is, raises several questions:
  1. what is the source of your data set?
  1. where is the data set (raw logs) ?
- it is also hard to grant some points if there is no unit test for the implementation of the PS service

# Execution

- **stops with the following error**: `{"init terminating in do_boot",{{badmatch,{error,{1,erl_parse,["syntax error before: ","7"]}}},[{init,start_it,1,[]},{init,start_em,1,[]},{init,do_boot,3,[]}]}}`
- after several changes I made the erlang code works, your code is only compatible with erlang/otp >= 21
- :warning: your project seems incomplete, contact me ASAP if this is not the case.

# Grade
| Bootstrap network (20%) | PS service implementation (50%) | Experimental scenario (30%) | Grade in % | Points (up to 5) |
|---|---|---|---|---|
|20|	30|	10|	60|	3|
