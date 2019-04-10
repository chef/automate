require 'spec_helper'
require 'ctl-helpers/ohai'
require 'ohai'

describe CtlHelpers::Ohai do
  describe "#attribute" do

    let(:ohai_double) { double("ohai") }
    before(:each) do
      CtlHelpers::Ohai.reset!
      expect(ohai_double).to receive(:all_plugins).once
      expect(::Ohai::System).to receive(:new).once.and_return(ohai_double)
      allow(ohai_double).to receive(:[]).with(String).and_return "hello"
    end

    it "initializes ohai and returns the requested attribute" do
      expect(CtlHelpers::Ohai.attribute('world')).to eq 'hello'
    end

    it "caches system attributes" do
      expect(CtlHelpers::Ohai.attribute('world')).to eq 'hello'
      expect(CtlHelpers::Ohai.attribute('universe')).to eq 'hello'
    end
  end
end
