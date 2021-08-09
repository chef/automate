require 'spec_helper'
require 'cluster'

describe AutomateCluster do
  it 'should return a output module object' do
    expect(AutomateCluster.terraform).to eq AutomateCluster::Terraform
  end
end
