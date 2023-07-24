package config

type Conf struct {
	Tcp Tcp `json:"tcp" mapstructure:"tcp"`
	Web
	SSDB SSDB `json:"ssdb" mapstructure:"ssdb"`
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
