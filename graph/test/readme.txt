This directory contains a tiny test program to show how to perform
local and remote graphing.

1. Compilation

In order to get a heap profile out of a program, we have to compile it
with some profiling options:

ghc --make heaptest -O2 -prof -auto-all

In short, the -prof option enables profiling, while -auto-all
instruments the executable by putting a cost centre (a named point of
measurement) at every top-level declaration. If you want more
fine-grained heap profiles, you can put SCC pragmas at any expression
within the program. Consult the documentation of GHC for further
details.

2. Local profiling

Local profiling is simple: just invoke the program through
hp2any-graph and pass it the necessary parameters:

hp2any-graph -e heaptest -- +RTS -hc -K100M -i0.02

Everything after the -- is passed to the slave process as command line
parameter. In this case, we are passing profiling related parameters
to the runtime (+RTS). In particular, we ask for a heap profile by
cost centre stack (-hc), set a big stack so the program can keep
running for a little while (-K), and set a profile sample rate that's
higher than the default (-i).

Don't be surprised if the animation is not smooth, as the heap profile
might be aggressively buffered by the operating system. When a program
is more complex and there are more active cost centres, one can see
the samples almost as they are produced. That's also the reason to
increase the sampling rate for this small example.

3. Remote profiling

In order to access the heap profile of a remote process, we need a
server that relays the information to the grapher. This is essentially
the same as above, except we use hp2any-relay and also specify a port
number to listen on:

hp2any-relay -p 5678 -e heaptest -- +RTS -hc -K100M -i0.02

We can connect to such a relay by starting the grapher in remote mode,
where we only give it a server address:

hp2any-graph -s localhost:5678

It is possible to attach several viewers to the same relay at the same
time. Each grapher sees only the samples produced after it was
attached.

4. Viewing later

The grapher is not capable of viewing heap profiles of processes that
already finished. The history manager (hp2any-manager, in a separate
package) takes care of that duty, providing a much more comfortable
interface.
