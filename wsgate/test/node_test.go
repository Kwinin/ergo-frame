package test

import (
	"flag"
	"fmt"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"github.com/ergo-services/ergo/proto/dist"
	"net"
	"testing"
)

func TestNode(t *testing.T) {
	if conn, err := net.Dial("tcp", ":25001"); err != nil {
		fmt.Println("Connect to the node' listening port FAILED")
		t.Fatal(err)
	} else {
		defer conn.Close()
	}

	if conn, err := net.Dial("tcp", fmt.Sprintf(":%d", dist.DefaultEPMDPort)); err != nil {
		fmt.Println("Connect to the node' listening EPMD port FAILED")
		t.Fatal(err)
	} else {
		defer conn.Close()
	}
}

func TestSpawnInf(t *testing.T) {
	type Person struct {
		Name string
		Age  int
	}
	// 使用接口类型来存储不同类型的值
	var i interface{} = Person{Name: "Alice", Age: 30}

	// 使用类型断言将接口类型转换为自定义结构体类型
	if p, ok := i.(Person); ok {
		fmt.Println("类型断言成功！")
		fmt.Println("Name:", p.Name)
		fmt.Println("Age:", p.Age)
	} else {
		fmt.Println("类型断言失败！")
	}
}

type testMonitor struct {
	gen.Server
	v chan interface{}
}

func TestNodeProxyConnect(t *testing.T) {
	flag.Parse()

	fmt.Printf("Starting node: node1 (cluster 1) ...")
	node1, err := ergo.StartNode("node1@localhost", "secret1", node.Options{})
	if err != nil {
		panic(err)
	}
	fmt.Println("OK")

	fmt.Printf("Starting node: node2 (cluster 1) with Proxy.Transit = true ...")
	opts2 := node.Options{}
	opts2.Proxy.Transit = true
	node2, err := ergo.StartNode("node2@localhost", "secret1", opts2)
	if err != nil {
		panic(err)
	}
	fmt.Println("OK")

	fmt.Printf("Starting node: node3 (cluster 2) with Proxy.Transit = true ...")
	opts3 := node.Options{}
	opts3.Proxy.Transit = true
	node3, err := ergo.StartNode("node3@localhost", "secret2", opts3)
	if err != nil {
		panic(err)
	}
	fmt.Println("OK")

	proxyCookie := "abc"
	fmt.Printf("Starting node: node4 (cluster 2) with Proxy.Cookie = %q ...", proxyCookie)
	opts4 := node.Options{}
	opts4.Proxy.Cookie = proxyCookie
	opts4.Proxy.Accept = true
	node4, err := ergo.StartNode("node4@localhost", "secret2", opts4)
	if err != nil {
		panic(err)
	}
	fmt.Println("OK")

	fmt.Printf("Add static route on node2 to node3 with custom cookie to get access to the cluster 2 ...")
	routeOptions := node.RouteOptions{
		Cookie: "secret2",
	}
	if err := node2.AddStaticRouteOptions(node3.Name(), routeOptions); err != nil {
		panic(err)
	}
	fmt.Println("OK")

	fmt.Printf("Add proxy route to node4 via node2 on node1 with proxy cookie = %q and enabled encryption ...", proxyCookie)
	proxyRoute1 := node.ProxyRoute{
		Name:   node4.Name(),
		Proxy:  node2.Name(),
		Cookie: proxyCookie,
		Flags:  node.DefaultProxyFlags(),
	}
	proxyRoute1.Flags.EnableEncryption = true
	if err := node1.AddProxyRoute(proxyRoute1); err != nil {
		panic(err)
	}
	fmt.Println("OK")

	fmt.Printf("Add proxy route to node4 via node3 on node2 ...")
	proxyRoute2 := node.ProxyRoute{
		Name:  node4.Name(),
		Proxy: node3.Name(),
	}
	if err := node2.AddProxyRoute(proxyRoute2); err != nil {
		panic(err)
	}
	fmt.Println("OK")

	fmt.Printf("Connect node1 to node4 ...")
	if err := node1.Connect(node4.Name()); err != nil {
		panic(err)
	}
	fmt.Println("OK")
	fmt.Println("Peers on node1", node1.Nodes())
	fmt.Println("Peers on node2", node2.Nodes())
	fmt.Println("Peers on node3", node3.Nodes())
	fmt.Println("Peers on node4", node4.Nodes())

	fmt.Println("Peers on node1", node1.NodesIndirect())
	fmt.Println("Peers on node2", node2.NodesIndirect())
	fmt.Println("Peers on node3", node3.NodesIndirect())
	fmt.Println("Peers on node4", node4.NodesIndirect())

	node1.Stop()
	node2.Stop()
	node3.Stop()
	node4.Stop()
}
