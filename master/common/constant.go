package common

const (
	GateType   string = "gateway" //网关
	ServerType string = "server"  //服务
	DBType     string = "db"      //数据
)

const (
	MasterGenServer string = "master" //主节点
	WgateGenServer  string = "wgate"  //网关节点
	GamerGenServer  string = "gamer"  //游戏节点
	CMDGenServer    string = "cmd"    //cmd节点
)

const (
	StartSuccess string = "StartSuccess"
	Shutdown     string = "Shutdown"
	OpenConn     string = "OpenConn"
	CloseConn    string = "CloseConn"
	Register     string = "Register"
)

const (
	NodeStatusOnline  int = 1
	NodeStatusOffLine int = 2
	NodeStatusDisable int = 3
)
