package common

type GenNodeType string

const (
	GateType   GenNodeType = "gateway" //网关
	ServerType GenNodeType = "server"  //服务
	DBType     GenNodeType = "db"      //数据
)

type GenServerName string

const (
	MasterGenServer GenServerName = "master" //主节点
	WgateGenServer  GenServerName = "wgate"  //网关节点
	GamerGenServer  GenServerName = "gamer"  //游戏节点
	CMDGenServer    GenServerName = "cmd"    //cmd节点
)

type CommandMsg string

const (
	StartSuccess CommandMsg = "StartSuccess"
	Shutdown     CommandMsg = "Shutdown"
	OpenConn     CommandMsg = "OpenConn"
	CloseConn    CommandMsg = "CloseConn"
)
