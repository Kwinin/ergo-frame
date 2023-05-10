// port.go

package main

import (
	"bufio"
	"os"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	writer := bufio.NewWriter(os.Stdout)
	for {
		buff := make([]byte, 2)
		_, err := reader.Read(buff)
		if err != nil {
			panic(err.Error())
		}
		switch int(buff[0]) {
		case 1:
			buff[1] = byte(foo(int(buff[1])))
		case 2:
			buff[1] = byte(bar(int(buff[1])))
		}
		writer.Write(buff[1:2])
		// 这里需要注意，要用Flush进行处理，否则erlang端收不到信息
		writer.Flush()
	}
}

func foo(num int) int {
	return num + 1
}

func bar(num int) int {
	return 2 * num
}
