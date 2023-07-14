package main

import (
	"fmt"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
)

func main() {
	node1opts := node.Options{}
	node1opts.Proxy.Flags = node.DefaultProxyFlags()
	node1opts.Proxy.Flags.EnableRemoteSpawn = false

	node1, _ := ergo.StartNode("node1remoteSpawn@k8smaster", "", node1opts)
	node2opts := node.Options{}
	node2opts.Proxy.Transit = true
	node2opts.Proxy.Accept = true
	node2, _ := ergo.StartNode("node2remoteSpawn@k8smaster", "", node2opts)
	node3opts := node.Options{}
	node3opts.Proxy.Accept = true
	node3opts.Proxy.Transit = true
	node3, _ := ergo.StartNode("node3remoteSpawn@k8smaster", "", node3opts)

	fmt.Printf("kwinin %s, %s\n", node3.Name(), node2.Name())
	route := node.ProxyRoute{
		Name:  "master_core@k8smaster",
		Proxy: node2.Name(),
	}
	node1.AddProxyRoute(route)
	defer node1.Wait()
	defer node2.Wait()
	defer node3.Wait()

	if err := node1.Connect("master_core@k8smaster"); err != nil {
		fmt.Println(111, err)
	}

	node2.ProvideRemoteSpawn("master_holder_server", &demo{})
	process, err := node1.Spawn("gs1", gen.ProcessOptions{}, &demo{})
	if err != nil {
		fmt.Println(222, err)
	}

	opts := gen.RemoteSpawnOptions{
		Name: "master_holder_server",
	}
	fmt.Println("    process gs1@node1 request to spawn new process on node2 and register this process with name 'remote': ")
	gotPid, err := process.RemoteSpawn("master_core@k8smaster", "master_holder_server", opts, etf.Term(etf.Atom("Kwinin Hello")))
	if err != nil {
		fmt.Println(333, err)
	}

	fmt.Println("OK", process.Name(), process.Self(), gotPid)

}

type demo struct {
	gen.Server
}

func (d *demo) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	fmt.Printf("[%s] HandleCast: %#v\n", process.Name(), message)
	switch message {
	case etf.Atom("stop"):
		return gen.ServerStatusStopWithReason("stop they said")
	}
	return gen.ServerStatusOK
}

func (d *demo) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	fmt.Printf("[%s] HandleCall: %#v, From: %s\n", process.Name(), message, from.Pid)

	switch message.(type) {
	case etf.Atom:
		return "hello", gen.ServerStatusOK

	default:
		return message, gen.ServerStatusOK
	}
}

func (d *demo) Terminate(process *gen.ServerProcess, reason string) {
	fmt.Printf("[%s] Terminating process with reason %q", process.Name(), reason)
}
