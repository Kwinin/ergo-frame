package helper

import (
	"bytes"
	"encoding/binary"
)

func IsValueExists(value string, array []string) bool {
	for _, v := range array {
		if v == value {
			return true
		}
	}
	return false
}

// IntToBytes int 转换为[]byte
func IntToBytes(i int32, packet int32) []byte {
	var buf = make([]byte, 2)
	if packet == 2 {
		binary.BigEndian.PutUint16(buf, uint16(i))
	} else {
		binary.BigEndian.PutUint32(buf, uint32(i))
	}
	return buf
}

// BytesCombine 多个[]byte数组合并成一个[]byte
func BytesCombine(pBytes ...[]byte) []byte {
	len := len(pBytes)
	s := make([][]byte, len)
	for index := 0; index < len; index++ {
		s[index] = pBytes[index]
	}
	sep := []byte("")
	return bytes.Join(s, sep)
}
