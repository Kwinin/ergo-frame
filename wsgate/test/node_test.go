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
