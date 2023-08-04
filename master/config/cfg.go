package config

type Conf struct {
	SSDB     SSDB     `json:"ssdb" mapstructure:"ssdb"`
	NodeList NodeList `json:"nodeList" mapstructure:"nodeList"`
}

type SSDB struct {
	Host string `mapstructure:"host"`
	Port int    `mapstructure:"port"`
}

type NodeList struct {
	Gamer  []NodeConf `mapstructure:"gamer"`
	WsGate []NodeConf `mapstructure:"wsgate"`
	Master []NodeConf `mapstructure:"master"`
}

type NodeConf struct {
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
