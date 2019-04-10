require "spec_helper"
require 'automate_ctl'

describe AutomateCtl do
  it "has a version number" do
    expect(described_class::VERSION).not_to be nil
  end
end
