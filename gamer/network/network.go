package network

import (
	"context"
	"encoding/binary"
	"io"
	"net"
	"sync"
	"sync/atomic"
	"time"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/node"
	"github.com/sirupsen/logrus"
)

// NetInterface network
type NetInterface interface {
	Start(n *NetWorkx)
	Close()
}

// NetWorkx 网络管理
type NetWorkx struct {
	//tcp/udp/kcp
	src NetInterface

	//包长度0 2 4
	Packet int32
	//读取超时时间(秒)
	Readtimeout int32

	MsgTime int32
	MsgNum  int32

	//tcp kcp
	NetType string
	//监听端口.
	Port int32
	//用户对象池  //nw.UserPool.Get().(*client).OnConnect()
	UserPool sync.Pool
	//CreateGenServerObj func() genServer.GateGenHanderInterface

	//启动成功后回调
	StartHook func()

	//新连接回调
	connectHook func()
	//连接关闭回调
	closedConnectHook func()
	//socket 关闭回调
	closeHook func()

	ConnectCount  int32 //连接数
	MaxConnectNum int32 //最大连接数
	OpenConn      bool  //是否开启连接

	gateNode node.Node
}

// NewNetWorkX    instance
func NewNetWorkX(createObj func() genServer.GateGenHanderInterface, port, packet, readtimeout int32, nettype string, maxConnectNum, msgtime, msgnum int32,
	startHook, closeHook, connectHook, closedConnectHook func()) *NetWorkx {

	netWorkx := &NetWorkx{
		Packet:  packet,
		NetType: nettype,
		Port:    port,
		UserPool: sync.Pool{
			New: func() interface{} {
				return createObj()
			}},
		//CreateGenServerObj: createObj,
		Readtimeout:       readtimeout,
		MsgTime:           msgtime,
		MsgNum:            msgnum,
		StartHook:         startHook,
		closeHook:         closeHook,
		connectHook:       connectHook,
		closedConnectHook: closedConnectHook,
		MaxConnectNum:     maxConnectNum,
		OpenConn:          false,
	}
	atomic.StoreInt32(&netWorkx.ConnectCount, 0)

	return netWorkx
}

// HandleClient 消息处理
func (n *NetWorkx) HandleClient(conn net.Conn) {
	process, clientHander, sendchan, err := n.CreateProcess()
	if err != nil {
		logrus.Error("createProcess err: [%v]", err)
		return
	}

	n.onConnect()
	atomic.AddInt32(&n.ConnectCount, 1)
	defer atomic.AddInt32(&n.ConnectCount, -1)

	defer n.UserPool.Put(clientHander)
	defer n.onClosedConnect()
	defer conn.Close()

	defer process.Send(process.Self(), etf.Term(etf.Tuple{etf.Atom("$gen_cast"), etf.Atom("SocketStop")}))

	rootContext := context.Background()
	sendctx, sendcancelFunc := context.WithCancel(rootContext)
	defer sendcancelFunc()

	go func(conn net.Conn) {
		for {
			select {
			case buf := <-sendchan:
				le := tools.IntToBytes(int32(len(buf)), n.Packet)
				conn.Write(tools.BytesCombine(le, buf))
			case <-sendctx.Done():
				//logrus.Debug("exit role sendGO")
				return
			}
		}
	}(conn)

	unix := time.Now().Unix()
	msgNum := 0
	for {
		//超时
		if n.Readtimeout != 0 {
			readtimeout := time.Second * time.Duration(n.Readtimeout)
			conn.SetReadDeadline(time.Now().Add(readtimeout))
		}

		_, buf, e := UnpackToBlockFromReader(conn, n.Packet)
		if e != nil {
			switch e {
			case io.EOF:
				//logrus.Debug("socket closed:", e.Error())
			default:
				logrus.Warn("socket closed:", e.Error())
			}
			return
		}
		//readchan <- buf

		if len(buf) < int(n.Packet+4) {
			logrus.Debug("buf len:", len(buf), n.Packet+4, len(buf[n.Packet:]))
			return
		}

		module := int32(binary.BigEndian.Uint16(buf[n.Packet : n.Packet+2]))
		method := int32(binary.BigEndian.Uint16(buf[n.Packet+2 : n.Packet+4]))
		//process.Send(process.Self(), etf.Tuple{module, method, buf[n.Packet+4:]})
		logrus.Debug("客户端发消息:  ", process.Name())
		err := process.Send(process.Self(), etf.Term(etf.Tuple{etf.Atom("$gen_cast"), etf.Tuple{module, method, buf[n.Packet+4:]}}))
		if err != nil {
			logrus.Warnf("send error:[%v] [%v] [%v]", method, err.Error(), n.ConnectCount)
			return
		}

		//间隔时间大于 N 分钟后 或者 接收到500条消息后 给连接送条信息
		now := time.Now().Unix()
		msgNum++

		if now > unix+int64(n.MsgTime) || msgNum >= int(n.MsgNum) {
			//logrus.Infof("time:=======>[%v] [%v]", time.Now().Format("15:04:05"), msgNum)

			process.Send(process.Self(), etf.Term(etf.Tuple{etf.Atom("$gen_cast"), "timeloop"}))
			//process.Send(process.Self(), "timeloop")

			//gamechan <- commonstruct.ProcessMsg{MsgType: commonstruct.ProcessMsgTimeInterval, Data: msgNum}
			unix = now
			msgNum = 0
		}
	}

}
