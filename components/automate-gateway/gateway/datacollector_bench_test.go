package gateway

import (
	"testing"

	ingestProto "github.com/chef/automate/api/external/ingest/request"
	complianceEvent "github.com/chef/automate/components/compliance-service/ingest/events/compliance"
)

func init() {
	loadRawExamples()
}

var (
	gRun      *ingestProto.Run
	gAction   *ingestProto.Action
	gLiveness *ingestProto.Liveness
	gReport   *complianceEvent.Report
)

//////////////////////////////////////////
//    Benchmark for ChefRun Messages    //
//////////////////////////////////////////
func BenchmarkMessageConvertionUnmarshalProtoFromBytesChefRun(b *testing.B) {
	var run ingestProto.Run
	var err error
	for r, body := range rawruns {
		b.Run("chef_run proto unmarshal "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				err = UnmarshalProtoFromBytes(body, &run)
				if err != nil {
					b.Errorf("Error unmarshaling:  %v", err)
				}
			}
		})
	}
	gRun = &run
}
func BenchmarkMessageConvertionParseBytesToChefRun(b *testing.B) {
	var run *ingestProto.Run
	for r, body := range rawruns {
		b.Run("chef_run proto parsing "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				run = ParseBytesToChefRun(body)
			}
		})
	}
	gRun = run
}

//////////////////////////////////////////
//  Benchmark for ChefAction Messages   //
//////////////////////////////////////////
func BenchmarkMessageConvertionUnmarshalProtoFromBytesChefAction(b *testing.B) {
	var action ingestProto.Action
	var err error
	for r, body := range rawactions {
		b.Run("chef_action proto unmarshal "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				err = UnmarshalProtoFromBytes(body, &action)
				if err != nil {
					b.Errorf("Error unmarshaling:  %v", err)
				}
			}
		})
	}
	gAction = &action
}
func BenchmarkMessageConvertionParseBytesToChefAction(b *testing.B) {
	var action *ingestProto.Action
	for r, body := range rawactions {
		b.Run("chef_action proto parsing "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				action = ParseBytesToChefAction(body)
			}
		})
	}
	gAction = action
}

//////////////////////////////////////////
//   Benchmark for Liveness Messages    //
//////////////////////////////////////////
func BenchmarkMessageConvertionUnmarshalProtoFromBytesLivenessPing(b *testing.B) {
	var liveness ingestProto.Liveness
	var err error
	for r, body := range rawliveness {
		b.Run("liveness_ping proto unmarshal "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				err = UnmarshalProtoFromBytes(body, &liveness)
				if err != nil {
					b.Errorf("Error unmarshaling:  %v", err)
				}
			}
		})
	}
	gLiveness = &liveness
}
func BenchmarkMessageConvertionParseBytesToLivenessPing(b *testing.B) {
	var liveness *ingestProto.Liveness
	for l, body := range rawliveness {
		b.Run("liveness_ping proto parsing "+l, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				liveness = ParseBytesToLivenessPing(body)
			}
		})
	}
	gLiveness = liveness
}

////////////////////////////////////////////////
//  Benchmark for ComplianceReport Messages   //
////////////////////////////////////////////////
func BenchmarkMessageConvertionUnmarshalProtoFromBytesComplianceReport(b *testing.B) {
	var report complianceEvent.Report
	var err error
	for r, body := range rawreports {
		b.Run("compliance_report proto unmarshal "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				err = UnmarshalProtoFromBytes(body, &report)
				if err != nil {
					b.Errorf("Error unmarshaling:  %v", err)
				}
			}
		})
	}
	gReport = &report
}
func BenchmarkMessageConvertionParseBytesToComplianceReport(b *testing.B) {
	var report *complianceEvent.Report
	for l, body := range rawreports {
		b.Run("compliance_report proto parsing "+l, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				report = ParseBytesToComplianceReport(body)
			}
		})
	}
	gReport = report
}
