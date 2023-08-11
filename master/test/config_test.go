package test

import (
	"fmt"
	"master/config"
	"testing"
)

func TestConfig(t *testing.T) {
	config.InitConfig("./../conf")
	node, err := config.GetNodeInfo("gamer", 2)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Printf("node %+v", node.Addr)
}
