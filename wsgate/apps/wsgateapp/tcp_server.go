package wsgateapp

import (
	"crypto/tls"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/lib"
	"wsgate/config"
)

func createTcpActor() gen.ServerBehavior {
	return &tcpServer{}
}

type tcpServer struct {
	gen.TCP
}

func (ts *tcpServer) InitTCP(process *gen.TCPProcess, args ...etf.Term) (gen.TCPOptions, error) {
	options := gen.TCPOptions{
		Host:    config.Cfg.Tcp.Host,
		Port:    uint16(config.Cfg.Tcp.Port),
		Handler: &tcpHandler{},
	}

	if config.Cfg.Tcp.Enable {
		cert, _ := lib.GenerateSelfSignedCert("localhost")
		logger.Println("TLS enabled. Generated self signed certificate. You may check it with command below:")
		logger.Infof("   $ openssl s_client -connect %s:%d\n", config.Cfg.Tcp.Host, config.Cfg.Tcp.Port)
		options.TLS = &tls.Config{
			Certificates:       []tls.Certificate{cert},
			InsecureSkipVerify: true,
		}
	}

	return options, nil
}
