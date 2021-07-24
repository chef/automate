package util

import (
	"testing"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/stretchr/testify/assert"
)

const (
	sampleReport = `  <Node>
      <ID>ReportID</ID>
      <NodeID>ID1</NodeID>
      <NodeName>Node1</NodeName>
      <EndTime>
          <Seconds>-62135596800</Seconds>
      </EndTime>
      <Platform>
          <Platform></Platform>
      </Platform>
      <Environment>test-env</Environment>
      <IPAddress>10.23.149.1</IPAddress>
      <FQDN>api.example.com</FQDN>
      <Profiles>
          <Profile>
              <Name>ProfileName</Name>
              <Title>Profile, Title</Title>
              <Version>1.2.3</Version>
              <Summary>Profile summary</Summary>
              <Controls>
                  <Control>
                      <ID>ControlID1</ID>
                      <Title>Control Title</Title>
                      <Impact>0.123</Impact>
                      <Waiver></Waiver>
                      <Results>
                          <Result>
                              <Status>passed</Status>
                              <RunTime>12345</RunTime>
                              <CodeDescription>super complex code</CodeDescription>
                              <Message>all done</Message>
                              <SkipMessage>we don&#39;t skip</SkipMessage>
                          </Result>
                      </Results>
                  </Control>
              </Controls>
          </Profile>
      </Profiles>
  </Node>`
)

func TestEmptyExportToXML(t *testing.T) {
	data, err := ReportToXML(&reportingapi.Report{})
	output := "  <Node>\n      <EndTime></EndTime>\n      <Platform>\n          <Platform></Platform>\n      </Platform>\n      <Profiles></Profiles>\n  </Node>"
	assert.Nil(t, err, "ReportToXML with empty report works.")
	assert.Equal(t, output, string(data), "ReportToXML with empty list of reports works.")
}

func TestNilExportToXML(t *testing.T) {
	_, err := ReportToXML(nil)
	assert.NotNil(t, err, "ReportToXML with nil report throws an error.")
}

func TestExportToXMLWithData(t *testing.T) {
	report := getSampleReport()
	data, err := ReportToXML(report)
	assert.Nil(t, err, "ReportToXML with a simple report generates without errors.")
	assert.Equal(t, sampleReport, string(data), "ReportToXML with simple report works.")
}
