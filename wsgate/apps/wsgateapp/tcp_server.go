package wsgateapp

import (
	"crypto/tls"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/lib"
	"wsgate/config"
	"wsgate/log"
)

func createTcpActor() gen.ServerBehavior {
	return &tcpServer{}
}

type tcpServer struct {
	gen.TCP
}

func (ts *tcpServer) InitTCP(process *gen.TCPProcess, args ...etf.Term) (gen.TCPOptions, error) {
	options := gen.TCPOptions{
		Host:    config.ServerCfg.Tcp.Host,
		Port:    uint16(config.ServerCfg.Tcp.Port),
		Handler: &tcpHandler{},
	}

	if config.ServerCfg.Tcp.Enable {
		cert, _ := lib.GenerateSelfSignedCert("localhost")
		log.Logger.Println("TLS enabled. Generated self signed certificate. You may check it with command below:")
		log.Logger.Infof("   $ openssl s_client -connect %s:%d\n", config.ServerCfg.Tcp.Host, config.ServerCfg.Tcp.Port)
		options.TLS = &tls.Config{
			Certificates:       []tls.Certificate{cert},
			InsecureSkipVerify: true,
		}
	}

	return options, nil
}
