package config

type Conf struct {
	NodeList []NodeConf `json:"nodeList" mapstructure:"nodeList"`
}

type NodeConf struct {
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
