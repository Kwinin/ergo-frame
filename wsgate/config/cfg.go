package config

type Conf struct {
	Node       NodeConf `json:"node" mapstructure:"node"`
	MasterAddr string
	Tcp        Tcp `json:"tcp" mapstructure:"tcp"`
	Web
}

type Tcp struct {
	Host   string `mapstructure:"host"`
	Port   int    `mapstructure:"port"`
	Enable bool   `mapstructure:"enable"`
}

type Web struct {
	Host   string `mapstructure:"host"`
	Port   int    `mapstructure:"port"`
	Enable bool   `mapstructure:"enable"`
}

type SSDB struct {
	Host string `mapstructure:"host"`
	Port int    `mapstructure:"port"`
}

type NodeConf struct {
	Id   int32  `mapstructure:"id"`
	Role string `mapstructure:"role"`
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
