package config

type Conf struct {
	Tcp Tcp `json:"tcp" mapstructure:"tcp"`
	Web
	SSDB     SSDB       `json:"ssdb" mapstructure:"ssdb"`
	NodeList []NodeConf `json:"nodeList" mapstructure:"nodeList"`
	Cookie   string     `json:"cookie" mapstructure:"cookie"`
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
	Name string `mapstructure:"name"`
	Addr string `mapstructure:"addr"`
	Sign string `mapstructure:"sign"`
	Ip   string `mapstructure:"ip"`
}
