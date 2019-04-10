package protobuf

import "github.com/golang/protobuf/proto"

func Convert(pb proto.Message, out proto.Message) error {
	inBuf, err := proto.Marshal(pb)
	if err != nil {
		return err
	}

	err = proto.Unmarshal(inBuf, out)
	if err != nil {
		return err
	}
	return nil
}

type fn func() (proto.Message, error)

func CallDomainService(in proto.Message, inDomain proto.Message, backendCall fn, out proto.Message) error {
	err := Convert(in, inDomain)
	if err != nil {
		return err
	}
	domainOut, err := backendCall()
	if err != nil {
		return err
	}
	err = Convert(domainOut, out)
	if err != nil {
		return err
	}
	return nil
}
