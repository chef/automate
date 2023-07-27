package genconfig

import (
	"fmt"

	"github.com/chef/automate/lib/pmt"
)

type AwsRegions interface {
	Choose(label string) (regionVal string, err error)
}

type AwsRegionsImp struct {
	Prompt pmt.Prompt
}

func AwsRegionsImpFactory(p pmt.Prompt) *AwsRegionsImp {
	return &AwsRegionsImp{
		Prompt: p,
	}
}

type awsRegionItem struct {
	Label string
	Key   string
}

var AwsRegionsList = []awsRegionItem{
	{"US East (Ohio) us-east-2", "us-east-2"},
	{"US East (N. Virginia) us-east-1", "us-east-1"},
	{"US West (N. California) us-west-1", "us-west-1"},
	{"US West (Oregon) us-west-2", "us-west-2"},
	{"Africa (Cape Town) af-south-1", "af-south-1"},
	{"Asia Pacific (Hong Kong) ap-east-1", "ap-east-1"},
	{"Asia Pacific (Hyderabad) ap-south-2", "ap-south-2"},
	{"Asia Pacific (Jakarta) ap-southeast-3", "ap-southeast-3"},
	{"Asia Pacific (Melbourne) ap-southeast-4", "ap-southeast-4"},
	{"Asia Pacific (Mumbai) ap-south-1", "ap-south-1"},
	{"Asia Pacific (Osaka) ap-northeast-3", "ap-northeast-3"},
	{"Asia Pacific (Seoul) ap-northeast-2", "ap-northeast-2"},
	{"Asia Pacific (Singapore) ap-southeast-1", "ap-southeast-1"},
	{"Asia Pacific (Sydney) ap-southeast-2", "ap-southeast-2"},
	{"Asia Pacific (Tokyo) ap-northeast-1", "ap-northeast-1"},
	{"Canada (Central) ca-central-1", "ca-central-1"},
	{"Europe (Frankfurt) eu-central-1", "eu-central-1"},
	{"Europe (Ireland) eu-west-1", "eu-west-1"},
	{"Europe (London) eu-west-2", "eu-west-2"},
	{"Europe (Milan) eu-south-1", "eu-south-1"},
	{"Europe (Paris) eu-west-3", "eu-west-3"},
	{"Europe (Spain) eu-south-2", "eu-south-2"},
	{"Europe (Stockholm) eu-north-1", "eu-north-1"},
	{"Europe (Zurich) eu-central-2", "eu-central-2"},
	{"Middle East (Bahrain) me-south-1", "me-south-1"},
	{"Middle East (UAE) me-central-1", "me-central-1"},
	{"South America (São Paulo) sa-east-1", "sa-east-1"},
	{"AWS GovCloud (US-East) us-gov-east-1", "us-gov-east-1"},
	{"AWS GovCloud (US-West) us-gov-west-1", "us-gov-west-1"},
}

func (r *AwsRegionsImp) genListMap() (list []string, findMap map[string]string) {
	list = []string{}
	findMap = map[string]string{}
	for _, v := range AwsRegionsList {
		k := fmt.Sprintf("%v %v", v.Label, v.Key)
		list = append(list, k)
		if _, ok := findMap[k]; !ok {
			findMap[k] = v.Key
		}
	}
	return
}

func (r *AwsRegionsImp) Choose(label string) (regionVal string, err error) {
	awsRegionOptions, findMap := r.genListMap()
	_, region, err := r.Prompt.SelectSearch(label, awsRegionOptions...)
	if err != nil {
		return
	}
	regionVal = findMap[region]
	return
}
