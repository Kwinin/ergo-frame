package helper

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"io"
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

func UnpackToBlockFromReader(reader io.Reader, packet int32) (int32, []byte, error) {
	if reader == nil {
		return 0, nil, errors.New("reader is nil")
	}
	var info = make([]byte, packet)
	if e := readUntil(reader, info); e != nil {
		if e == io.EOF {
			return 0, nil, e
		}
		return 0, nil, e //errorx.Wrap(e)
	}

	length, e := LengthOf(info, packet)
	if e != nil {
		return 0, nil, e
	}
	var content = make([]byte, length)
	if e := readUntil(reader, content); e != nil {
		if e == io.EOF {
			return 0, nil, e
		}
		return 0, nil, e //errorx.Wrap(e)
	}

	return length, append(info, content...), nil
}

func readUntil(reader io.Reader, buf []byte) error {
	if len(buf) == 0 {
		return nil
	}
	var offset int
	for {
		n, e := reader.Read(buf[offset:])
		if e != nil {
			if e == io.EOF {
				return e
			}
			return e //errorx.Wrap(e)
		}
		//logrus.Debugf("offset:[%s]  buf[%s]", offset, len(buf))
		offset += n
		if offset >= len(buf) {
			break
		}
	}
	return nil
}

// LengthOf Length of the stream starting validly.
// Length doesn't include length flag itself, it refers to a valid message length after it.
func LengthOf(stream []byte, packet int32) (int32, error) {
	if len(stream) < int(packet) {
		return 0, errors.New(fmt.Sprint("stream lenth should be bigger than ", packet))
	}

	switch packet {
	case 2:
		return int32(binary.BigEndian.Uint16(stream[0:2])), nil
	case 4:
		return int32(binary.BigEndian.Uint32(stream[0:4])), nil
	default:
		errstr := fmt.Sprintf("stream lenth seting error  [packet: %v]", packet)
		return 0, errors.New(errstr)
	}

}

// ==========msg register =======
// 消息注册
func CreateRegisterFunc[T any](execfunc func(*T)) func(buf []byte) {
	return func(buf []byte) {
		info := new(T)
		err := decodeProto(info, buf)
		if err != nil {
			logrus.Errorf("decode error[%v]", err.Error())
		} else {
			//logrus.Debugf("client msg:[%v] [%v]", info, tools.GoID())
			execfunc(info)
		}
	}
}

// protobuf 解码
func decodeProto(info interface{}, buf []byte) error {
	if data, ok := info.(protoreflect.ProtoMessage); ok {
		return proto.Unmarshal(buf, data)
	}
	return nil
}
