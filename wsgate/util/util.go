package util

import (
	"fmt"
	"github.com/seefan/gossdb"
	"reflect"
	"strconv"
	"strings"
)

func StructToMap(data interface{}) map[string]interface{} {
	dataType := reflect.TypeOf(data)
	dataValue := reflect.ValueOf(data)

	if dataType.Kind() == reflect.Ptr {
		dataType = dataType.Elem()
		dataValue = dataValue.Elem()
	}

	if dataType.Kind() != reflect.Struct {
		return nil
	}

	result := make(map[string]interface{})

	for i := 0; i < dataType.NumField(); i++ {
		//field := dataType.Field(i)
		//fieldValue := dataValue.Field(i).Interface()
		//result[field.Tag.Get("json")] = fieldValue

		// 将字段值存入map，使用Interface()获取对应的interface{}
		field := dataType.Field(i)
		fieldValue := dataValue.Field(i)
		switch fieldValue.Kind() {
		case reflect.Bool:
			result[field.Name] = fieldValue.Bool()
		case reflect.Slice:
			sliceData := make([]interface{}, fieldValue.Len())
			for j := 0; j < fieldValue.Len(); j++ {
				sliceData[j] = fieldValue.Index(j).Interface()
			}
			result[field.Name] = sliceData
		default:
			result[field.Name] = fieldValue.Interface()
		}
	}

	return result
}

func MapToStruct(data map[string]gossdb.Value, target interface{}) (interface{}, error) {
	targetValue := reflect.ValueOf(target)
	if targetValue.Kind() != reflect.Ptr || targetValue.IsNil() {
		return nil, fmt.Errorf("target must be a non-nil pointer to a struct")
	}

	elem := targetValue.Elem()
	if elem.Kind() != reflect.Struct {
		return nil, fmt.Errorf("target must be a pointer to a struct")
	}

	for key, value := range data {
		field := elem.FieldByName(key)
		if !field.IsValid() {
			continue
		}

		fieldType := field.Type()

		switch fieldType.Kind() {
		case reflect.String:
			field.SetString(value.String())
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			num, err := strconv.ParseInt(value.String(), 10, 64)
			if err != nil {
				return nil, err
			}
			field.SetInt(num)
		case reflect.Bool:
			b, err := strconv.ParseBool(value.String())
			if err != nil {
				return nil, err
			}
			field.SetBool(b)
		case reflect.Slice:
			arr := strings.Split(strings.Trim(value.String(), "[]"), ",")
			slice := reflect.MakeSlice(fieldType, len(arr), len(arr))
			for i := 0; i < len(arr); i++ {
				num, err := strconv.ParseInt(arr[i], 10, 64)
				if err != nil {
					return nil, err
				}
				slice.Index(i).SetInt(num)
			}
			field.Set(slice)
		// Add more cases for other field types, if needed
		default:
			// 未处理的类型，忽略或处理为你想要的默认值
		}
	}

	return target, nil
}
