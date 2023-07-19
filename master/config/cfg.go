package config

var Cfg Conf

type Conf struct {
	DATABASE struct {
		Url      string `mapstructure:"url"`
		Username string `mapstructure:"username"`
		Password string `mapstructure:"password"`
	}
}
