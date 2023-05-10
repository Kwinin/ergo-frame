package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net"
)

func main() {
	listener, err := net.Listen("tcp", "localhost:8080")
	if err != nil {
		log.Print(err)
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Print(err) // e.g., connection aborted
			continue
		}
		handleConn(conn) // handle one connection at a time
	}
}

func handleConn(conn net.Conn) {
	defer conn.Close()
	result, err := ioutil.ReadAll(conn) //获得收到的数据

	if err != nil {
		log.Fatal(err)
	}
	for index, tb := range result {
		fmt.Printf("index=%d, content=%c\n", index, tb)
	}
	// fmt.Printf(string(result))
	fmt.Println("=============================================")
}
