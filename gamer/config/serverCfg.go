package config

type ServerConfig struct {
	ServerID    int32    `json:"serverId"`
	ServerRole  string   `json:"serverRole"`
	Version     string   `json:"version"`
	Cookie      string   `json:"cookie"`
	SSDB        SSDBConf `json:"ssdb" mapstructure:"ssdb"`
	Node        NodeConf `json:"node" mapstructure:"node"`
	MasterAddr  string   `json:"masterAddr"`
	ListenBegin uint16   `json:"listenBegin"`
	ListenEnd   uint16   `json:"listenEnd"`
}

type NodeConf struct {
	Id   int32  `mapstructure:"id"`
	Role string `mapstructure:"role"`
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}

type SSDBConf struct {
	Host string `mapstructure:"host"`
	Port int    `mapstructure:"port"`
}
