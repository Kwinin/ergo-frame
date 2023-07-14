package player

import (
	"fmt"
	"gamer/apps/gamerapp/helper"
	"gamer/apps/gamerapp/player/mod"
	"github.com/ergo-services/ergo/etf"
	"reflect"
)

type Server struct {
	Custom
}

func (md *Server) InitCustom(process *CustomProcess, args ...etf.Term) error {
	fmt.Printf("Started instance of MyCustom with PID %s and args %v\n", process.Self(), args)
	LoopMod()
	return nil
}

func (md *Server) HandleHello(process *CustomProcess) CustomStatus {
	fmt.Println("got Hello")
	return CustomStatusOK
}

func (md *Server) HandleCustomDirect(process *CustomProcess, message interface{}) (interface{}, error) {

	fmt.Println("Say hi to increase counter twice")
	process.Hi()
	return nil, nil
}

type ModInf interface {
	Name() string
	OnDate() string
}

func LoopMod() {
	Names := []string{"Name"}
	LoopModByMethods(Names)
}

func LoopModByMethods(methods []string) {
	mods := []ModInf{
		mod.Attr{}, mod.Shop{},
	}
	for _, animal := range mods {
		modValue := reflect.ValueOf(animal)
		animalType := modValue.Type()

		for i := 0; i < animalType.NumMethod(); i++ {
			method := animalType.Method(i)
			methodValue := modValue.MethodByName(method.Name)

			if methodValue.IsValid() && helper.IsValueExists(method.Name, methods) {
				result := methodValue.Call([]reflect.Value{})
				fmt.Println(222, result)
			}
		}
	}
}
