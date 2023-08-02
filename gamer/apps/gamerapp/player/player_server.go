package player

import (
	"gamer/apps/gamerapp/helper"
	"gamer/apps/gamerapp/player/mod"
	"gamer/common"
	"gamer/log"
	"github.com/ergo-services/ergo/etf"
	"reflect"
)

type Server struct {
	Custom
	common.GbVar
}

func (md *Server) InitCustom(process *CustomProcess, args ...etf.Term) error {
	log.Logger.Infof("Started instance of MyCustom with PID %s and args %v\n", process.Self(), args)
	md.LoopMod()
	return nil
}

func (md *Server) HandleHello(process *CustomProcess) CustomStatus {
	log.Logger.Info("got Hello")
	return CustomStatusOK
}

func (md *Server) HandleCustomDirect(process *CustomProcess, message interface{}) (interface{}, error) {
	log.Logger.Info("Say hi to increase counter twice")
	process.Hi()
	return nil, nil
}

type ModInf interface {
	Name() string
	OnDate() string
}

func (md *Server) LoopMod() {
	Names := []string{"Name"}
	md.LoopModByMethods(Names)
}

func (md *Server) LoopModByMethods(methods []string) {
	mods := []ModInf{
		mod.Attr{
			common.GbVar{
				NodeName: md.NodeName,
				Cfg:      md.Cfg,
				DB:       md.DB,
			},
		},
		mod.Shop{
			common.GbVar{
				NodeName: md.NodeName,
				Cfg:      md.Cfg,
				DB:       md.DB,
			},
		},
	}
	for _, animal := range mods {
		modValue := reflect.ValueOf(animal)
		animalType := modValue.Type()

		for i := 0; i < animalType.NumMethod(); i++ {
			method := animalType.Method(i)
			methodValue := modValue.MethodByName(method.Name)

			if methodValue.IsValid() && helper.IsValueExists(method.Name, methods) {
				result := methodValue.Call([]reflect.Value{})
				log.Logger.Infof("result %+v", result)
			}
		}
	}
}
