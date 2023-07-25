package config

type Conf struct {
	SSDB SSDB `json:"ssdb" mapstructure:"ssdb"`
}

type SSDB struct {
	Host string `mapstructure:"host"`
	Port int    `mapstructure:"port"`
}
