package test

import (
	"fmt"
	"github.com/ergo-services/ergo/lib"
	"net"
	"strconv"
	"testing"
	"time"
)

func TestTcp(t *testing.T) {
	var connection net.Conn
	var err error

	hostPort := net.JoinHostPort("127.0.0.1", strconv.Itoa(4343))
	dialer := net.Dialer{}

	connection, err = dialer.Dial("tcp", hostPort)

	if err != nil {
		return
	}

	defer connection.Close()

	for i := 0; i < 5; i++ {
		str := lib.RandomString(16)

		fmt.Printf("send string %q to %q\n", str, connection.RemoteAddr().String())
		connection.Write([]byte(str))
		time.Sleep(time.Second)
	}
}
