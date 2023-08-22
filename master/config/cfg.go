package config

type Conf struct {
	NodeList []NodeConf `json:"nodeList" mapstructure:"nodeList"`
}

type NodeConf struct {
	Id   int32  `mapstructure:"id"`
	Role string `mapstructure:"role"`
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
