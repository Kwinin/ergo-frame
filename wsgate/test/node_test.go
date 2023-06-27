package test

import (
	"fmt"
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
