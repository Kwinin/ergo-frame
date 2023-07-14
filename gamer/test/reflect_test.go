package test

import (
	"fmt"
	"reflect"
	"testing"
)

func Add(a, b int) int {
	return a + b
}

func TestReflect(t *testing.T) {
	//funcName := "Add"
	funcValue := reflect.ValueOf(Add) // 获取函数的反射值

	arguments := []reflect.Value{
		reflect.ValueOf(23), // 第一个参数
		reflect.ValueOf(3),  // 第二个参数
	}

	result := funcValue.Call(arguments) // 调用函数

	fmt.Println(result[0].Int()) // 获取函数返回值
}

func TestPointer(t *testing.T) {
	var funcPtr func(int, int) int
	funcPtr = Add

	result := funcPtr(22, 3)

	fmt.Println(result)
}

type Animal interface {
	Speak() string
}

type Dog struct{}

func (d Dog) Speak() string {
	return "狗!"
}

func (d Dog) Run() {
	fmt.Println("Dog is running")
}

type Cat struct{}

func (c Cat) Speak() string {
	return "猫!"
}

func (c Cat) Sleep() {
	fmt.Println("Cat is sleeping")
}

func TestReflect2(t *testing.T) {
	animals := []Animal{Dog{}, Cat{}}

	for _, animal := range animals {
		//fmt.Println(animal.Speak())

		//// 类型断言
		//if dog, ok := animal.(Dog); ok {
		//	dog.Run()
		//}

		// 反射
		animalValue := reflect.ValueOf(animal)
		animalType := animalValue.Type()

		for i := 0; i < animalType.NumMethod(); i++ {
			method := animalType.Method(i)
			fmt.Println(method.Index, method.Name)
			methodValue := animalValue.MethodByName(method.Name)

			if methodValue.IsValid() {
				result := methodValue.Call([]reflect.Value{})
				fmt.Println(222, result)
			}

		}
	}
}
