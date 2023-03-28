package verification_test

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"sync"
	"testing"

	"github.com/chef/automate/lib/reporting"
	"github.com/chef/automate/lib/verification"
)

func TestRun(t *testing.T) {
	// reportChan := make(chan reporting.VerfictionReport, 5)
	// wg := &sync.WaitGroup{}
	// wg.Add(1)
	// go automateData(reportChan, wg)
	// for n := range reportChan {
	// 	fmt.Println(n.Report.StatusMessage)
	// }
	// wg.Wait()
	// close(reportChan)
	verification.VerifyCertificates("abc")
}

func automateData(reportChan chan reporting.VerfictionReport, wg *sync.WaitGroup) {
	defer wg.Done()
	data, err := ioutil.ReadFile("data.json")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	var dataStruct []reporting.Info

	err = json.Unmarshal(data, &dataStruct)
	if err != nil {
		fmt.Println("Error unmarshaling JSON:", err)
		return
	}
	for _, value := range dataStruct {
		//time.Sleep(time.Millisecond * 10000)

		myReport := reporting.VerfictionReport{
			TableKey:     "ChefServer",
			Report:       value,
			TotalReports: 4,
		}
		reportChan <- myReport
	}
}
