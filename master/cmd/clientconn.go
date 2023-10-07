/*
Copyright © 2022 NAME HERE <EMAIL ADDRESS>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package cmd

import (
	"context"
	"fmt"
	"master/helper"
	"master/proto/account"

	"net"
	"strconv"
	"sync"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"google.golang.org/protobuf/proto"
)

var wg sync.WaitGroup

// clientconnCmd represents the clientconn command
var clientconnCmd = &cobra.Command{
	Use:   "clientconn",
	Short: "模拟客户端连接",
	Long:  `模拟客户端连接  args: 连接数量`,
	Run: func(cmd *cobra.Command, args []string) {
		num := 2

		wg = sync.WaitGroup{}

		if len(args) == 1 {
			num, _ = strconv.Atoi(args[0])
			num++
		}
		wg.Add(num - 1)
		fmt.Println(num)
		for i := 1; i < num; i++ {
			go conn(i)
		}
		wg.Wait()
	},
}

func init() {
	rootCmd.AddCommand(clientconnCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// clientconnCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// clientconnCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}

func conn(key int) {
	defer wg.Done()

	tcpaddr, _ := net.ResolveTCPAddr("tcp", "127.0.0.1:3344")
	conn, err := net.DialTCP("tcp", nil, tcpaddr)
	if err != nil {
		fmt.Println("Dial failed:", err)
		return
	}
	defer conn.Close()

	sendctx, sendcancelFunc := context.WithCancel(context.Background())
	defer sendcancelFunc()
	sendchan := make(chan []byte, 1)

	accountname := fmt.Sprintf("%v_%v", "test", key)
	password := "123456"

	msg := &account.C2S_Login{
		Account:  accountname,
		Password: password,
	}
	SendToClient(int32(account.MSG_ACCOUNT_MODULE), int32(account.MSG_ACCOUNT_LOGIN), msg, sendchan)

	for {
		select {
		case buf := <-sendchan:
			le := helper.IntToBytes(int32(len(buf)), 2)
			conn.Write(helper.BytesCombine(le, buf))
		case <-sendctx.Done():
			return
		}
	}

}

// //SendToClient 发送消息至客户端
func SendToClient(module int32, method int32, pb proto.Message, sendchan chan []byte) {
	//logrus.Debugf("client send msg [%v] [%v] [%v]", module, method, pb)
	data, err := proto.Marshal(pb)
	if err != nil {
		logrus.Errorf("proto encode error[%v] [%v][%v] [%v]", err.Error(), module, method, pb)
		return
	}
	mldulebuf := helper.IntToBytes(module, 2)
	methodbuf := helper.IntToBytes(method, 2)
	sendchan <- helper.BytesCombine(mldulebuf, methodbuf, data)
}
