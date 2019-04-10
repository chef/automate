require 'spec_helper.rb'

require 'runner/delete/config'
require 'runner/exceptions'

require 'ctl-helpers/prompt'

describe Runner::Delete::Config do
  let(:args) { [] }
  let(:config) {
    described_class.new(args, "description")
  }

  context "#validate!" do
    context "when fqdn is not passed" do
      it "throws an error" do
        expect { config.validate! }.to raise_error(Runner::Exceptions::BadArgumentError)
      end
    end

    context "when fqdn is passed" do
      let(:fqdn) { "some_fqdn" }
      let(:args) { [fqdn] }

      it "assigns a value for a fqdn" do
        config.validate!
        expect(config.fqdn).to eq(fqdn)
      end

      context "when enterprise is passed" do
        let(:ent) { "some_enterprise" }
        let(:args) { [fqdn, "--enterprise", ent] }

        it "assigns a value for enterprise" do
          config.validate!
          expect(config.enterprise).to eq(ent)
        end
      end

      context "when enterprise flag is passed in but enterprise is not" do
        let(:args) { [fqdn, "--enterprise"] }

        it "throws an error" do
          expect { config.validate! }.to raise_error(Runner::Exceptions::BadArgumentError)
        end
      end
    end
  end

  context "#confirm_config" do
    let(:sudo) { true }
    it "records sudo" do
      expect(config).to receive(:display_config)
      expect(config).to receive(:prompt_for_config)
      config.confirm_config()
    end
  end
end
