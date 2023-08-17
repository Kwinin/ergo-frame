package config

type Conf struct {
	Node       NodeConf `json:"node" mapstructure:"node"`
	MasterAddr string
}

type NodeConf struct {
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
