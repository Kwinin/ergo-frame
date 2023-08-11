package config

type Conf struct {
	SSDB     SSDB       `json:"ssdb" mapstructure:"ssdb"`
	NodeList []NodeConf `json:"nodeList" mapstructure:"nodeList"`
}

type SSDB struct {
	Host string `mapstructure:"host"`
	Port int    `mapstructure:"port"`
}

type NodeConf struct {
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
